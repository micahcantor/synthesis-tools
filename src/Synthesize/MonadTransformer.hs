{-# HLINT ignore "Eta reduce" #-}

module Synthesize.MonadTransformer where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Maybe as Maybe
import Data.Traversable (for)
import GHC (Ghc, HValue, LHsExpr, TyCon, Type)
import qualified GHC
import qualified GHC.Core.Type as Type
import GHC.Hs (GhcPs)
import qualified GHC.Hs.Utils as Hs.Utils
import GHC.Stack (HasCallStack)
import qualified GHC.Types.Var as Var
import qualified GHC.Utils.Outputable as Outputable
import Synthesize.GHC
import Text.Printf (printf)

-- Try to get an unwrapping function expression for a given type
getUnwrappingFunctionExpr :: HasCallStack => Type -> Ghc TypedExpr
getUnwrappingFunctionExpr stackType = do
  let stackTyCon = Maybe.fromJust $ tyToTyCon (removeForAll stackType)
  idents <- getBindingIdsInScope
  let types = map Var.varType idents
  let unwrapperIdentTypes = [(ident, ty) | (ident, ty) <- zip idents types, isUnwrappingType ty stackTyCon]
  unwrapperExprs <- for unwrapperIdentTypes $ \(ident, ty) -> do
    expr <- identToExpr ident
    pure (TypedExpr expr ty)
  pure (head unwrapperExprs)
  where
    -- Is a TyCon the first argument of a given type?
    isUnwrappingType :: Type -> TyCon -> Bool
    isUnwrappingType ty tyCon =
      case Type.splitFunTy_maybe (removeForAll ty) of
        Just (_, argType, _) -> case tyToTyCon argType of
          Just argTyCon -> lookupTyConSynonym tyCon == lookupTyConSynonym argTyCon
          Nothing -> False
        Nothing -> False

buildUnwrapperApplication :: LHsExpr GhcPs -> [TypedExpr] -> Ghc (LHsExpr GhcPs)
buildUnwrapperApplication stackExpr unwrapperTypedExprs = do
  holeExpr <- getHoleExpr
  pure (go holeExpr unwrapperTypedExprs)
  where
    go holeExpr exprs = case exprs of
      [] -> stackExpr
      (TypedExpr expr ty) : typedExprs ->
        let arity = getArity ty
         in if arity == 1
              then Hs.Utils.mkHsApp expr (go holeExpr typedExprs)
              else
                let holes = replicate (arity - 1) holeExpr
                 in Hs.Utils.mkHsApps expr (holes ++ [go holeExpr typedExprs])

-- Synthesize an expression to run a monad stack, given the target expression and type
synthesizeRunStack :: LHsExpr GhcPs -> Type -> Ghc HValue
synthesizeRunStack stackExpr stackType = do
  identityTyCon <- getIdentityTyCon
  ioTyCon <- getIOTyCon
  unwrappers <- getUnwrappers stackType ioTyCon identityTyCon
  app <- buildUnwrapperApplication stackExpr (reverse unwrappers)
  liftIO (printf "Generated expr: %s\n" (show (Outputable.ppr app)))
  GHC.compileParsedExpr app
  where
    -- Recursively get unwrapping functions for the stack
    getUnwrappers :: Type -> TyCon -> TyCon -> Ghc [TypedExpr]
    getUnwrappers stackType' ioTyCon identityTyCon
      | Just ioTyCon == tyToTyCon stackType' = pure []
      | Just identityTyCon == tyToTyCon stackType' = do
          unwrapIdentity <- getRunIdentityTypedExpr
          pure [unwrapIdentity]
      | otherwise = do
          unwrapper <- getUnwrappingFunctionExpr stackType'
          innerUnwrappers <- getUnwrappers (getInnerMonad stackType') ioTyCon identityTyCon
          pure (unwrapper : innerUnwrappers)

makeRunStack :: String -> Ghc HValue
makeRunStack stackName = do
  stackExpr <- GHC.parseExpr stackName
  stackType <- GHC.exprType GHC.TM_Inst stackName
  synthesizeRunStack stackExpr stackType

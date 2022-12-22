module Synthesize.MonadTransformer where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (..))
import GHC (Ghc, LHsExpr, TyCon, Type)
import qualified GHC
import GHC.Core.TyCo.Ppr (appPrec)
import GHC.Driver.Monad (GhcMonad)
import GHC.Hs (GhcPs)
import qualified GHC.Hs.Utils as Hs.Utils
import qualified GHC.Utils.Outputable as Outputable
import Synthesize.GHC
import Synthesize.Monad (SynthesisError (..), SynthesizeM, runSynthesizeM)

-- Try to get an unwrapping function expression for a given type
getUnwrappingFunctionExpr :: String -> TyCon -> [TypedExpr] -> SynthesizeM TypedExpr
getUnwrappingFunctionExpr functionName stackTyCon typedExprs = do
  let unwrapperExprs = filter (\(TypedExpr expr ty) -> isUnwrapper expr ty stackTyCon) typedExprs
  case unwrapperExprs of
    [] -> throwError (NoUnwrapperFound (show (Outputable.ppr stackTyCon)))
    (unwrapper : _) -> pure unwrapper
  where
    -- Is a TyCon the first argument of a given type?
    isUnwrapper :: LHsExpr GhcPs -> Type -> TyCon -> Bool
    isUnwrapper expr ty tyCon =
      let exprStr = show (Outputable.ppr expr)
       in exprStr /= functionName
            && case getArgType ty of
              Just argType -> case tyToTyCon argType of
                Just argTyCon -> lookupTyConSynonym tyCon == lookupTyConSynonym argTyCon
                Nothing -> False
              Nothing -> False

buildUnwrapperApplication :: GhcMonad m => [TypedExpr] -> LHsExpr GhcPs -> m (LHsExpr GhcPs)
buildUnwrapperApplication unwrapperTypedExprs paramExpr = do
  holeExpr <- getHoleExpr
  pure $ GHC.stripParensLHsExpr (go holeExpr unwrapperTypedExprs)
  where
    go holeExpr exprs = case exprs of
      [] -> paramExpr
      (TypedExpr expr ty) : typedExprs ->
        let arity = getArity ty
         in if arity == 1
              then GHC.parenthesizeHsExpr appPrec (Hs.Utils.mkHsApp expr (go holeExpr typedExprs))
              else
                let holes = replicate (arity - 1) holeExpr
                 in GHC.parenthesizeHsExpr appPrec (Hs.Utils.mkHsApps expr (go holeExpr typedExprs : holes))

-- Synthesize an expression to run a monad stack, given the target stack type and param expression
synthesizeRunStack :: String -> Type -> LHsExpr GhcPs -> SynthesizeM String
synthesizeRunStack functionName stackType paramExpr = do
  identityTyCon <- getIdentityTyCon
  ioTyCon <- getIOTyCon
  typedExprs <- getTypedExprsInScope
  unwrappers <- getUnwrappers stackType ioTyCon identityTyCon typedExprs
  app <- buildUnwrapperApplication (reverse unwrappers) paramExpr
  pure (show (Outputable.ppr app))
  where
    -- Recursively get unwrapping functions for the stack
    getUnwrappers :: Type -> TyCon -> TyCon -> [TypedExpr] -> SynthesizeM [TypedExpr]
    getUnwrappers stackType' ioTyCon identityTyCon typedExprs = do
      stackTyCon <- case tyToTyCon stackType' of
        Just tyCon -> pure tyCon
        Nothing -> throwError (InvalidTarget (show (Outputable.ppr stackType')))
      if ioTyCon == stackTyCon
        then pure []
        else
          if identityTyCon == stackTyCon
            then do
              unwrapIdentity <- getRunIdentityTypedExpr
              pure [unwrapIdentity]
            else do
              unwrapper <- getUnwrappingFunctionExpr functionName stackTyCon typedExprs
              innerMonad <- case getInnerMonad stackType' of
                Just m -> pure m
                Nothing -> throwError (InvalidTarget (show (Outputable.ppr stackType')))
              innerUnwrappers <- getUnwrappers innerMonad ioTyCon identityTyCon typedExprs
              pure (unwrapper : innerUnwrappers)

makeRunStack :: String -> String -> Ghc (Either SynthesisError String)
makeRunStack functionName paramName = runSynthesizeM $ do
  parseResult <- GHC.parseName functionName
  case parseResult of
    [] -> throwError (UnknownTarget functionName)
    _ -> do
      functionType <- GHC.exprType GHC.TM_Inst functionName
      paramExpr <- GHC.parseExpr paramName
      case getArgType functionType of
        Just stackType -> synthesizeRunStack functionName stackType paramExpr
        Nothing -> throwError (InvalidTarget functionName)

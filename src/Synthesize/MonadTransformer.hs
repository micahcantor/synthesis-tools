{-# HLINT ignore "Eta reduce" #-}

module Synthesize.MonadTransformer where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Maybe as Maybe
import GHC (Ghc, HValue, LHsExpr, TyCon, Type)
import qualified GHC
import qualified GHC.Core.Type as Type
import GHC.Hs (GhcPs)
import qualified GHC.Hs.Utils as Utils
import GHC.Stack (HasCallStack)
import qualified GHC.Types.Var as Var
import qualified GHC.Utils.Outputable as Outputable
import Synthesize.GHC

getUnwrappingFunctionExpr :: HasCallStack => Type -> Ghc (LHsExpr GhcPs)
getUnwrappingFunctionExpr stackType = do
  let stackTyCon = Maybe.fromJust $ tyToTyCon (removeForAll stackType)
  idents <- getBindingIdsInScope
  let types = map Var.varType idents
  let unwrapperIdents = [ident | (ident, ty) <- zip idents types, isUnwrappingType ty stackTyCon]
  liftIO (print (map Outputable.ppr unwrapperIdents))
  unwrapperExprs <- traverse identToExpr unwrapperIdents
  pure (head unwrapperExprs)
  where
    isUnwrappingType :: Type -> TyCon -> Bool
    isUnwrappingType ty tyCon =
      case Type.splitFunTy_maybe (removeForAll ty) of
        Just (_, argType, _) -> case tyToTyCon argType of
          Just argTyCon -> lookupTyConSynonym tyCon == lookupTyConSynonym argTyCon
          Nothing -> False
        Nothing -> False

synthesizeRunStack :: LHsExpr GhcPs -> Type -> Ghc HValue
synthesizeRunStack stackExpr stackType = do
  identityTyCon <- getIdentityTyCon
  ioTyCon <- getIOTyCon
  unwrappers <- getUnwrappers stackType ioTyCon identityTyCon
  let app = foldr Utils.mkHsApp stackExpr (reverse unwrappers)
  liftIO (print (Outputable.ppr app))
  GHC.compileParsedExpr app
  where
    -- recursively get unwrapping functions for the stack
    getUnwrappers :: Type -> TyCon -> TyCon -> Ghc [LHsExpr GhcPs]
    getUnwrappers stackType' ioTyCon identityTyCon
      | Just ioTyCon == tyToTyCon stackType' = pure []
      | Just identityTyCon == tyToTyCon stackType' = do
          unwrapIdentity <- getUnwrapIdentity
          pure [unwrapIdentity]
      | otherwise = do
          unwrapper <- getUnwrappingFunctionExpr stackType'
          innerUnwrappers <- getUnwrappers (getInnerMonad stackType') ioTyCon identityTyCon
          pure (unwrapper : innerUnwrappers)

makeRunStack :: String -> Ghc HValue
makeRunStack stackName = do
  stackExpr <- GHC.parseExpr stackName
  stackType <- GHC.exprType GHC.TM_Inst stackName
  liftIO (print (Outputable.ppr stackExpr, Outputable.ppr stackType))
  synthesizeRunStack stackExpr stackType

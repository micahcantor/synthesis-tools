{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Synthesize.MonadTransformer where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Traversable (for)
import GHC (Ghc, HscEnv, LHsExpr, TyCon, Type)
import qualified GHC
import GHC.Driver.Monad (GhcMonad)
import GHC.Driver.Session (HasDynFlags)
import GHC.Hs (GhcPs)
import qualified GHC.Hs.Utils as Hs.Utils
import qualified GHC.Types.Var as Var
import GHC.Utils.Logger (HasLogger, Logger)
import qualified GHC.Utils.Outputable as Outputable
import Synthesize.GHC
import GHC.Core.TyCo.Ppr (appPrec)
import Control.Monad.IO.Class (MonadIO)

data SynthesisError
  = UnknownTarget String
  | InvalidTarget String
  | NoUnwrapperFound String
  deriving (Show, Eq)

newtype SynthesizeM a = SynthesizeM {unSynthesizeM :: ExceptT SynthesisError Ghc a}
  deriving (Functor, Applicative, Monad, MonadError SynthesisError, MonadThrow, MonadCatch, MonadMask, MonadIO, HasDynFlags)

{- For some reason these two couldn't be automatically derived -}
instance HasLogger SynthesizeM where
  getLogger :: SynthesizeM Logger
  getLogger = SynthesizeM (lift GHC.getLogger)

instance GhcMonad SynthesizeM where
  getSession :: SynthesizeM HscEnv
  getSession = SynthesizeM (lift GHC.getSession)

  setSession :: HscEnv -> SynthesizeM ()
  setSession = SynthesizeM . lift . GHC.setSession

-- Isn't this ironic. Don't unwrap Ghc since we do that in Run.hs with the loaded environment
runSynthesizeM :: SynthesizeM a -> Ghc (Either SynthesisError a)
runSynthesizeM = runExceptT . unSynthesizeM

-- Try to get an unwrapping function expression for a given type
getUnwrappingFunctionExpr :: TyCon -> SynthesizeM TypedExpr
getUnwrappingFunctionExpr stackTyCon = do
  idents <- getBindingIdsInScope
  let types = map Var.varType idents
  let unwrapperIdentTypes = [(ident, ty) | (ident, ty) <- zip idents types, isUnwrappingType ty stackTyCon]
  unwrapperExprs <- for unwrapperIdentTypes $ \(ident, ty) -> do
    expr <- identToExpr ident
    pure (TypedExpr expr ty)
  case unwrapperExprs of
    [] -> throwError (NoUnwrapperFound (show (Outputable.ppr stackTyCon)))
    (unwrapper : _) -> pure unwrapper
  where
    -- Is a TyCon the first argument of a given type?
    isUnwrappingType :: Type -> TyCon -> Bool
    isUnwrappingType ty tyCon =
      case getArgType ty of
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
                 in GHC.parenthesizeHsExpr appPrec (Hs.Utils.mkHsApps expr (holes ++ [go holeExpr typedExprs]))

-- Synthesize an expression to run a monad stack, given the target function name and stack type
synthesizeRunStack :: Type -> LHsExpr GhcPs -> SynthesizeM String
synthesizeRunStack stackType paramExpr = do
  identityTyCon <- getIdentityTyCon
  ioTyCon <- getIOTyCon
  unwrappers <- getUnwrappers stackType ioTyCon identityTyCon
  app <- buildUnwrapperApplication (reverse unwrappers) paramExpr
  pure (show (Outputable.ppr app))
  where
    -- Recursively get unwrapping functions for the stack
    getUnwrappers :: Type -> TyCon -> TyCon -> SynthesizeM [TypedExpr]
    getUnwrappers stackType' ioTyCon identityTyCon = do
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
              unwrapper <- getUnwrappingFunctionExpr stackTyCon
              innerMonad <- case getInnerMonad stackType' of
                Just m -> pure m
                Nothing -> throwError (InvalidTarget (show (Outputable.ppr stackType')))
              innerUnwrappers <- getUnwrappers innerMonad ioTyCon identityTyCon
              pure (unwrapper : innerUnwrappers)

makeRunStack :: String -> String -> Ghc (Either SynthesisError String)
makeRunStack functionName paramName = runSynthesizeM $ do
  functionType <- GHC.exprType GHC.TM_Inst functionName
  paramExpr <- GHC.parseExpr paramName
  case getArgType functionType of
    Just stackType -> synthesizeRunStack stackType paramExpr
    Nothing -> throwError (InvalidTarget functionName)

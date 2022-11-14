{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Synthesize.MonadTransformer where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT, MonadError, catchError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Traversable (for)
import GHC (GhcT, HValue, HscEnv, LHsExpr, TyCon, Type)
import qualified GHC
import GHC.Driver.Monad (GhcMonad, liftGhcT)
import GHC.Driver.Session (HasDynFlags)
import GHC.Hs (GhcPs)
import qualified GHC.Hs.Utils as Hs.Utils
import qualified GHC.Paths
import qualified GHC.Types.Var as Var
import GHC.Utils.Logger (HasLogger)
import qualified GHC.Utils.Outputable as Outputable
import Synthesize.GHC
import Text.Printf (printf)

data SynthesisError
  = UnknownTarget String
  | InvalidTarget String
  | NoUnwrapperFound String
  deriving (Show, Eq)

newtype SynthesizeM a = SynthesizeM {unSynthesizeM :: GhcT (ExceptT SynthesisError IO) a}
  deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadMask, MonadIO, HasDynFlags, HasLogger, GhcMonad)

runSynthesizeM :: HscEnv -> SynthesizeM a -> IO (Either SynthesisError a)
runSynthesizeM env m = runExceptT $ GHC.runGhcT (Just GHC.Paths.libdir) $ do
  GHC.setSession env
  unSynthesizeM m

-- for some reason this can't be automatically derived
-- TODO: implement catchError
instance MonadError SynthesisError SynthesizeM where
  throwError :: SynthesisError -> SynthesizeM a
  throwError err = SynthesizeM (liftGhcT (throwError err))

  catchError :: SynthesizeM a -> (SynthesisError -> SynthesizeM a) -> SynthesizeM a
  catchError = undefined

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

buildUnwrapperApplication :: GhcMonad m => [TypedExpr] -> m (LHsExpr GhcPs)
buildUnwrapperApplication unwrapperTypedExprs = do
  holeExpr <- getHoleExpr
  pure (go holeExpr unwrapperTypedExprs)
  where
    go holeExpr exprs = case exprs of
      [] -> holeExpr
      (TypedExpr expr ty) : typedExprs ->
        let arity = getArity ty
         in if arity == 1
              then Hs.Utils.mkHsApp expr (go holeExpr typedExprs)
              else
                let holes = replicate (arity - 1) holeExpr
                 in Hs.Utils.mkHsApps expr (holes ++ [go holeExpr typedExprs])

-- Synthesize an expression to run a monad stack, given the target function name and stack type
synthesizeRunStack :: Type -> SynthesizeM HValue
synthesizeRunStack stackType = do
  identityTyCon <- getIdentityTyCon
  ioTyCon <- getIOTyCon
  unwrappers <- getUnwrappers stackType ioTyCon identityTyCon
  app <- buildUnwrapperApplication (reverse unwrappers)
  liftIO $ printf "Generated expr: %s\n" (show (Outputable.ppr app))
  GHC.compileParsedExpr app
  where
    -- Recursively get unwrapping functions for the stack
    getUnwrappers :: Type -> TyCon -> TyCon -> SynthesizeM [TypedExpr]
    getUnwrappers stackType' ioTyCon identityTyCon = do
      stackTyCon <- case tyToTyCon stackType' of
        Just tyCon -> pure tyCon
        Nothing -> throwError (InvalidTarget "<name>")
      if ioTyCon == stackTyCon then 
        pure []
      else if identityTyCon == stackTyCon then do
        unwrapIdentity <- getRunIdentityTypedExpr
        pure [unwrapIdentity]
      else do
        unwrapper <- getUnwrappingFunctionExpr stackTyCon
        innerUnwrappers <- getUnwrappers (getInnerMonad stackType') ioTyCon identityTyCon
        pure (unwrapper : innerUnwrappers)

makeRunStack :: HscEnv -> String -> IO (Either SynthesisError HValue)
makeRunStack env functionName = runSynthesizeM env $ do
  liftIO (printf "Synthesizing '%s'\n" functionName)
  functionType <- GHC.exprType GHC.TM_Inst functionName
  case getArgType functionType of
    Just stackType -> synthesizeRunStack stackType
    Nothing -> throwError (InvalidTarget functionName)

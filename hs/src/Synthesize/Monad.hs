{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Synthesize.Monad where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans (lift))
import GHC (Ghc, GhcMonad, HscEnv)
import qualified GHC
import GHC.Driver.Session (HasDynFlags)
import GHC.Utils.Logger (HasLogger, Logger)

data SynthesisError
  = UnknownTarget String
  | InvalidTarget String
  | NoUnwrapperFound String
  | NoSolutionFound String
  deriving (Eq)

instance Show SynthesisError where
  show err = case err of
    UnknownTarget name -> "Unknown target: '" <> name <> "'"
    InvalidTarget name -> "Invalid target: '" <> name <> "'"
    NoUnwrapperFound name -> "No unwrapper found in scope for '" <> name <> "'"
    NoSolutionFound msg -> "No solution found in scope for " <> msg

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
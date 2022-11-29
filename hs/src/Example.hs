module Example where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State
import Data.Functor.Identity (Identity (runIdentity))
import Control.Monad.State (StateT)

data Error

data Config

data MyState

config :: Config
config = undefined

type ErrorStack a = ExceptT Error Identity a

runWrong :: Int -> Int
runWrong x = undefined

-- correct: runIdentity (runExceptT m)
runErrorStack :: ErrorStack a -> Either Error a
runErrorStack m = runIdentity (runExceptT m)

type ReaderStack a = ReaderT Config (ExceptT Error Identity) a

-- correct: runIdentity (runExceptT (runReaderT m _))
runReaderStack :: ReaderStack a -> Either Error a
runReaderStack m = runIdentity (runExceptT (runReaderT m config))

type StateStack a = StateT MyState (ExceptT Error Identity) a

runStateStack :: StateStack a -> Either Error a
runStateStack = undefined

-- errors --

-- should throw error, invalid target
notUnwrapper :: Int -> Int
notUnwrapper _ = undefined

-- should throw error, invalid target
notFunction :: Int
notFunction = 5

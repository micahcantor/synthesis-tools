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

-- should throw error, invalid target
notUnwrapper :: Int -> Int
notUnwrapper _ = undefined

-- should throw error, invalid target
notFunction :: Int
notFunction = 5

runStackM :: ErrorStack a -> Either Error a
runStackM m = runIdentity (runExceptT m)

myErrorStack :: ErrorStack a
myErrorStack = undefined

type ReaderStack a = ReaderT Config (ExceptT Error Identity) a

runReaderStack :: ReaderStack a -> Either Error a
runReaderStack m = runIdentity (runExceptT (runReaderT undefined m))

myReaderStack :: ReaderStack a
myReaderStack = undefined

type StateStack a = StateT MyState (ExceptT Error Identity) a

myStateStack :: StateStack a
myStateStack = undefined

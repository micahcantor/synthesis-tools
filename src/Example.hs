module Example where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Functor.Identity (Identity (runIdentity))

data Error

data Config

config :: Config
config = undefined

type ErrorStack a = ExceptT Error Identity a

runStackM :: ErrorStack a -> Either Error a
runStackM m = runIdentity (runExceptT m)

myErrorStack :: ErrorStack a
myErrorStack = undefined

type ReaderStack a = ReaderT Config (ExceptT Error Identity) a

runReaderStack :: ReaderStack a -> Either Error a
runReaderStack m = runIdentity (runExceptT (runReaderT undefined m))

myReaderStack :: ReaderStack a
myReaderStack = undefined

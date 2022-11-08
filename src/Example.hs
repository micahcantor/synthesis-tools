module Example where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Functor.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT))

data Error

data Config

config :: Config
config = undefined

type StackM a = ExceptT Error Identity a

runStackM :: StackM a -> Either Error a
runStackM m = runIdentity (runExceptT m)

myStack :: StackM a
myStack = undefined

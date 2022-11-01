module Example where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Functor.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT))

data Error

data Config

config :: Config
config = undefined

type StackM a = ExceptT Error IO a

myStack :: StackM a
myStack = undefined

type Asdsd a = Identity a

runEval :: StackM a -> Either Error a



-- hello :: Int -> Identity Int
-- hello = Identity
somef :: Int -> Int
somef x = x + 1
runEval = undefined


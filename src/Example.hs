module Example where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Functor.Identity (Identity (runIdentity))

data Error

type StackM a = ExceptT Error Identity a

myStack :: StackM Int
myStack = pure 1

type Asdsd a = Identity a

runEval :: StackM a -> Either Error a
runEval m = runIdentity (runExceptT m)


-- hello :: Int -> Identity Int
-- hello = Identity
somef :: Int -> Int
somef x = x + 1

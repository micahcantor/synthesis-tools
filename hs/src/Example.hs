module Example where

import Control.Monad.Except (ExceptT, runExceptT, Except, runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State
import Data.Functor.Identity (Identity (runIdentity))
import Control.Monad.State (StateT)

data Error

data Config

data MyState



type ErrorStack a = ExceptT Error Identity a

myErrorStack :: ErrorStack Int
myErrorStack = undefined

-- correct: runIdentity (runExceptT m)
runErrorStack :: ErrorStack a -> Either Error a
runErrorStack m = runIdentity (runExceptT m)

data AppConfig
  
data AppError

type AppM a = ReaderT AppConfig (Except AppError) a

runAppM :: AppM a -> Either AppError a
runAppM m = runIdentity (runExceptT (runReaderT m initialConfig))

initialConfig :: AppConfig
initialConfig = undefined



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

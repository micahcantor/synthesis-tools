module Main where

import qualified Synthesize.Run
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName, functionName, paramName] -> Synthesize.Run.runSynthesis fileName functionName paramName
    _ -> putStrLn "Usage: ghc-hacking <file name> <function name> <parameter name>"

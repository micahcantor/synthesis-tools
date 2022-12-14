module Main where

import qualified Synthesize.Run
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName, functionName, paramName] -> Synthesize.Run.runSynthesis fileName functionName paramName
    [fileName, functionName] -> Synthesize.Run.runSynthesis fileName functionName "_param"
    _ -> putStrLn "Usage: synthesis-tools <file name> <function name> [<parameter name>]"

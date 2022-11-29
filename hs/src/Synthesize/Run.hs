module Synthesize.Run where

import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC (Ghc, HscEnv, runGhc)
import qualified GHC
import qualified GHC.Paths
import Synthesize.MonadTransformer (makeRunStack)
import qualified System.FilePath as FilePath
import System.IO (hPutStr, stderr)

-- Create a new GHC interactive session
initSession :: Ghc HscEnv
initSession = do
  dflags <- GHC.getSessionDynFlags
  GHC.setSessionDynFlags (dflags {GHC.ghcLink = GHC.LinkInMemory, GHC.backend = GHC.Interpreter})
  GHC.getSession

-- Run a Ghc action with a given environment
withEnv :: HscEnv -> Ghc a -> Ghc (HscEnv, a)
withEnv env action = do
  GHC.setSession env
  result <- action
  env' <- GHC.getSession
  pure (env', result)

-- Load a Haskell file into an interactive session
load :: String -> Ghc ()
load path = do
  target <- GHC.guessTarget path Nothing
  GHC.addTarget target
  loadResult <- GHC.load GHC.LoadAllTargets
  case loadResult of
    GHC.Succeeded -> do
      let loadedModule = GHC.IIModule (GHC.mkModuleName (FilePath.takeBaseName path))
      ctx <- GHC.getContext
      GHC.setContext (loadedModule : ctx)
    GHC.Failed ->
      liftIO $ hPutStr stderr ("Failed to load file: " <> path)

runSynthesis :: FilePath -> String -> String -> IO ()
runSynthesis fileName functionName paramName = GHC.runGhc (Just GHC.Paths.libdir) $ do
  initialEnv <- initSession
  (loadedEnv, _) <- withEnv initialEnv (load fileName)
  (_, synthesisResult) <- withEnv loadedEnv (makeRunStack functionName paramName)
  case synthesisResult of
    Left err -> liftIO (hPutStr stderr (show err))
    Right expr -> liftIO (putStr expr)
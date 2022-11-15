module Synthesize.Run where

import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC (Ghc, HscEnv, runGhc)
import qualified GHC
import qualified GHC.Paths
import Synthesize.MonadTransformer ( makeRunStack )
import qualified System.FilePath as FilePath

-- Create a new GHC interactive session
initSession :: IO HscEnv
initSession = GHC.runGhc (Just GHC.Paths.libdir) $ do
  dflags <- GHC.getSessionDynFlags
  GHC.setSessionDynFlags (dflags {GHC.ghcLink = GHC.LinkInMemory, GHC.backend = GHC.Interpreter})
  GHC.getSession

-- Run a Ghc action with a given environment
withEnv :: HscEnv -> Ghc () -> IO HscEnv
withEnv env action = GHC.runGhc (Just GHC.Paths.libdir) $ do
  GHC.setSession env
  action
  GHC.getSession

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
      liftIO $ putStrLn ("Successfully loaded file: " <> path)
    GHC.Failed ->
      liftIO $ putStrLn ("Failed to load file: " <> path)

runSynthesis :: FilePath -> String -> IO ()
runSynthesis fileName functionName = do
  initialEnv <- initSession
  loadedEnv <- withEnv initialEnv (load fileName)
  synthesisResult <- makeRunStack loadedEnv functionName
  print synthesisResult
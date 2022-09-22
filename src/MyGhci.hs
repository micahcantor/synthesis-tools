{-# LANGUAGE ScopedTypeVariables #-}

module MyGhci where

import Control.Exception (SomeException)
import qualified Control.Exception as Exception
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Dynamic as Dynamic
import qualified Data.List as List
import GHC (Ghc, HscEnv)
import qualified GHC
import qualified GHC.Paths
import qualified GHC.Types.Name as Name (getOccString)
import System.Console.Haskeline (InputT)
import qualified System.Console.Haskeline as Haskeline
import qualified System.FilePath as FilePath
import qualified Unsafe.Coerce as Coerce

initSession :: IO HscEnv
initSession = GHC.runGhc (Just GHC.Paths.libdir) $ do
  dflags <- GHC.getSessionDynFlags
  GHC.setSessionDynFlags (dflags {GHC.ghcLink = GHC.LinkInMemory, GHC.backend = GHC.Interpreter})
  GHC.setContext [GHC.IIDecl (GHC.simpleImportDecl (GHC.mkModuleName "Prelude"))]
  GHC.getSession

session :: HscEnv -> Ghc () -> IO HscEnv
session env action = GHC.runGhc (Just GHC.Paths.libdir) $ do
  GHC.setSession env
  action
  GHC.getSession

ghcCatch :: MonadIO m => IO a -> m (Maybe a)
ghcCatch action = liftIO $ do
  result <- Exception.try action
  case result of
    Left (err :: SomeException) -> do
      print err
      pure Nothing
    Right res -> pure (Just res)

addImport :: String -> Ghc ()
addImport name = do
  let importedModule = GHC.IIDecl (GHC.simpleImportDecl (GHC.mkModuleName name))
  ctx <- GHC.getContext
  GHC.setContext (importedModule : ctx)

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

browse :: Ghc ()
browse = do
  names <- GHC.getNamesInScope
  let nameStrings = map Name.getOccString names
  liftIO $ mapM_ putStrLn nameStrings

eval :: String -> Ghc ()
eval input = do
  dyn <- Dynamic.fromDynamic <$> GHC.dynCompileExpr input
  case dyn of
    Nothing -> do
      action <- GHC.compileExpr ("Prelude.print (" <> input <> ")")
      liftIO (Coerce.unsafeCoerce action)
    Just action -> liftIO action

parseCommand :: String -> Ghc ()
parseCommand cmd =
  case List.words cmd of
    (x : xs) -> case x of
      "import" -> addImport (List.concat xs)
      ":load" -> load (List.concat xs)
      ":browse" -> browse
      _ -> eval cmd
    [] -> pure ()

repl :: HscEnv -> InputT IO ()
repl env = do
  inputResult <- Haskeline.getInputLine ">>> "
  case inputResult of
    Nothing -> Haskeline.outputStrLn "Goodbye."
    Just input -> do
      let action = parseCommand input
      envResult <- ghcCatch (session env action)
      case envResult of
        Nothing -> repl env
        Just env' -> repl env'

startRepl :: IO ()
startRepl = do
  env <- initSession
  Haskeline.runInputT Haskeline.defaultSettings (repl env)

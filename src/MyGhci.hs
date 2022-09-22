{-# LANGUAGE ScopedTypeVariables #-}

module MyGhci where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Dynamic as Dynamic
import qualified Data.List as List
import GHC (Ghc, HscEnv)
import qualified GHC
import qualified GHC.Paths
import System.Console.Haskeline (InputT)
import qualified System.Console.Haskeline as Haskeline
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

addImport :: String -> Ghc ()
addImport name = do
  ctx <- GHC.getContext
  let importedModule = GHC.IIDecl (GHC.simpleImportDecl (GHC.mkModuleName name))
  GHC.setContext (importedModule : ctx)

ghcCatch :: MonadIO m => IO a -> m (Maybe a)
ghcCatch m = liftIO $ do
  result <- try m
  case result of
    Left (err :: SomeException) -> do
      print err
      pure Nothing
    Right res -> pure (Just res)

eval :: String -> Ghc ()
eval input = do
  dyn <- Dynamic.fromDynamic <$> GHC.dynCompileExpr input
  case dyn of
    Nothing -> do
      action <- GHC.compileExpr ("Prelude.print (" <> input <> ")")
      liftIO (Coerce.unsafeCoerce action)
    Just action -> liftIO action

repl :: HscEnv -> InputT IO ()
repl env = do
  inputResult <- Haskeline.getInputLine ">>> "
  case inputResult of
    Nothing -> pure ()
    Just input ->
      if "import" `List.isPrefixOf` input
        then do
          let name = List.concat (List.tail (List.words input))
          envResult <- ghcCatch (session env (addImport name))
          case envResult of
            Nothing -> repl env
            Just env' -> repl env'
        else do
          envResult <- ghcCatch (session env (eval input))
          case envResult of
            Nothing -> repl env
            Just env' -> repl env'

startRepl :: IO ()
startRepl = do
  env <- initSession
  Haskeline.runInputT Haskeline.defaultSettings (repl env)

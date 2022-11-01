{-# LANGUAGE ScopedTypeVariables #-}

module MyGhci where

import Control.Exception (SomeException)
import qualified Control.Exception as Exception
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Dynamic as Dynamic
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import GHC (Ghc, HscEnv)
import qualified GHC
import qualified GHC.Paths
import qualified GHC.Types.Name as Name
import qualified GHC.Types.TyThing as TyThing
import qualified GHC.Types.Var as Var
import System.Console.Haskeline (InputT)
import qualified System.Console.Haskeline as Haskeline
import qualified System.FilePath as FilePath
import qualified Unsafe.Coerce as Coerce
import qualified GHC.Utils.Outputable as Outputable
import qualified GHC.Core.Type as Type
import AutoMonadStack
import TypeConstraints
import GHC.Types.TyThing (TyThing)
import GHC.Core.TyCon (TyCon)

-- create a new GHC interactive session with Prelude pre-loaded
initSession :: IO HscEnv
initSession = GHC.runGhc (Just GHC.Paths.libdir) $ do
  dflags <- GHC.getSessionDynFlags
  GHC.setSessionDynFlags (dflags {GHC.ghcLink = GHC.LinkInMemory, GHC.backend = GHC.Interpreter})
  GHC.setContext [GHC.IIDecl (GHC.simpleImportDecl (GHC.mkModuleName "Prelude"))]
  GHC.getSession

-- Run a Ghc action with a given environment
withEnv :: HscEnv -> Ghc () -> IO HscEnv
withEnv env action = GHC.runGhc (Just GHC.Paths.libdir) $ do
  GHC.setSession env
  action
  GHC.getSession

-- run a Ghc action, catch and print any exception it throws
ghcCatch :: MonadIO m => IO a -> m (Maybe a)
ghcCatch action = liftIO $ do
  result <- Exception.try action
  case result of
    Left (err :: SomeException) -> do
      print err
      pure Nothing
    Right res -> pure (Just res)

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

-- List all available top-level names in the current interactive session.
browse :: Ghc ()
browse = do
  names <- filter Name.isValName <$> GHC.getNamesInScope
  tyThings <- Maybe.catMaybes <$> mapM GHC.lookupName names
  let types = map (Type.tidyTopType . Var.varType . TyThing.tyThingId) tyThings
  let typeStrings = map (show . Outputable.ppr) types
  let nameStrings = map Name.getOccString names
  let typeSignatures = zipWith (\name ty -> name <> " :: " <> ty) nameStrings typeStrings
  liftIO $ mapM_ putStrLn typeSignatures

browseTyCons :: Ghc ()
browseTyCons = do
  tyConNames <- filter Name.isTyConName <$> GHC.getNamesInScope
  tyThings <- Maybe.catMaybes <$> mapM GHC.lookupName tyConNames
  let tyCons = Maybe.mapMaybe tyThingToTyCon tyThings
  let tyConStrings = map (show . Outputable.ppr) tyCons
  liftIO $ mapM_ putStrLn tyConStrings
  where
    tyThingToTyCon :: TyThing -> Maybe TyCon
    tyThingToTyCon tyThing = case tyThing of
      GHC.ATyCon tyCon -> Just tyCon
      _ -> Nothing


printType :: String -> Ghc ()
printType name = do
  ty <- GHC.exprType GHC.TM_Inst name
  liftIO (print (Outputable.ppr ty))

-- Evaluate a Haskell expression or IO action in the current interactive session.
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
      ":browse-ty-cons" -> browseTyCons
      ":type" -> printType (List.concat xs)
      ":synth" -> do
        runStackValue <- makeRunStack (List.concat xs)
        liftIO (print runStackValue)
      _ -> eval cmd
    [] -> pure ()

repl :: HscEnv -> InputT IO ()
repl env = do
  inputResult <- Haskeline.getInputLine ">>> "
  case inputResult of
    Nothing -> Haskeline.outputStrLn "Goodbye."
    Just input -> do
      let action = parseCommand input
      envResult <- ghcCatch (withEnv env action)
      case envResult of
        Nothing -> repl env
        Just env' -> repl env'

startRepl :: IO ()
startRepl = do
  env <- initSession
  Haskeline.runInputT Haskeline.defaultSettings (repl env)

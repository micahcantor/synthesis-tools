module TypeConstraints where

  import GHC
  import qualified GHC.Types.Name as Name
  import qualified Data.Maybe as Maybe
  import qualified GHC.Types.TyThing as TyThing
  import qualified GHC.Types.Var as Var
  import qualified GHC.Core.Type as Type
  import qualified GHC.Utils.Outputable as Outputable
  import Control.Monad.IO.Class (MonadIO (liftIO))
  import GHC.Core.TyCo.Rep (Type(..))
  import GHC.Core.Type (TyCoVarBinder, eqType)
  import qualified GHC.Core.TyCo.Rep as Rep

  import GHC.Types.Var as Var


  satisfyTypeConstraints :: [a] -> [a] -> [a]
  satisfyTypeConstraints _ _ = []


  validOutput :: [Maybe (Mult, Type, Type)] -> [(Mult, Type, Type)]
  validOutput ls = do
    x <- ls
    maybe [] pure x


  findName :: String -> [String] -> String
  findName _ _ = ""

  getForAll :: Type -> Maybe (Id, Rep.ArgFlag)
  getForAll (ForAllTy (Var.Bndr x y) _) = Just (x ::Id, y)
  getForAll _ = Nothing


  removeForAllHelper :: Type -> Type
  removeForAllHelper t =
    case t of
      ForAllTy _ t1 -> removeForAllHelper t1
      t2 -> t2


  removeForAll :: [Type] -> [Type]
  removeForAll types = removeForAllHelper <$> types



  getTypeSigniture :: String -> GHC.Ghc ()

  getTypeSigniture _ =
    do
      names <- filter Name.isValName <$> GHC.getNamesInScope
      tyThings <- Maybe.catMaybes <$> mapM GHC.lookupName names
      let types = map (Type.tidyTopType . Var.varType . TyThing.tyThingId) tyThings
      let fa = map getForAll types
      _ <- liftIO $ mapM_ (print . Outputable.ppr . maybe [] pure . getForAll) types
      let comp = case (fa!!1, fa!!8) of
                  (Just (t1, _), Just (t2, _)) -> Just $ eqType (Var.varType t1) (Var.varType t2)
                  (_, _) -> Nothing


      let typeStrings = map (show . Outputable.ppr) $ removeForAll types
      let nameStrings = map Name.getOccString names
      let typeSignatures = zipWith (\name ty -> name <> " :: " <> ty) nameStrings typeStrings
      let primeTypeMaybe = map Type.splitFunTy_maybe $ removeForAll types
      -- let primeTypeMaybe = map Type.splitFunTy_maybe types
      let primeType = validOutput primeTypeMaybe
      let primeTypeString = map (\(m, t1, t2) -> (show . Outputable.ppr) m : (show . Outputable.ppr) t1 : [(show . Outputable.ppr) t2]) primeType

      _ <- liftIO $ mapM_ print primeTypeString

      -- liftIO $ mapM_ putStrLn $ show primeTypeString
      _ <- liftIO $ mapM_ putStrLn typeSignatures
      liftIO $ print comp
      -- pure ()

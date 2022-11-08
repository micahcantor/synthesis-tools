module Synthesize.GHC where

import qualified Data.Maybe as Maybe
import GHC (Ghc, LHsExpr, TyCon, Type)
import qualified GHC
import GHC.Core.TyCo.Rep (Type (..))
import qualified GHC.Core.TyCon as TyCon
import qualified GHC.Core.Type as Type
import GHC.Hs (GhcPs)
import GHC.Stack (HasCallStack)
import GHC.Types.Id (Id)
import qualified GHC.Types.Name as Name
import GHC.Types.TyThing (TyThing (..))
import qualified GHC.Types.TyThing as TyThing
import qualified GHC.Types.Var as Var
import qualified GHC.Utils.Outputable as Outputable

-- Import a package into an interactive session
addImport :: String -> Ghc ()
addImport name = do
  let importedModule = GHC.IIDecl (GHC.simpleImportDecl (GHC.mkModuleName name))
  ctx <- GHC.getContext
  GHC.setContext (importedModule : ctx)

tyThingTyCon :: TyThing -> Maybe TyCon
tyThingTyCon tyThing = case tyThing of
  ATyCon tyCon -> Just tyCon
  _ -> Nothing

tyToTyCon :: Type -> Maybe TyCon
tyToTyCon ty = case ty of
  TyConApp tyCon _ -> Just tyCon
  _ -> Nothing

identToExpr :: Id -> Ghc (LHsExpr GhcPs)
identToExpr = GHC.parseExpr . show . Outputable.ppr . Var.varName

removeForAll :: Type -> Type
removeForAll t = case t of
  ForAllTy _ t1 -> removeForAll t1
  _ -> t

-- Get a TyCon in the current scope, throw if not found
getTyConInScope :: HasCallStack => String -> Ghc TyCon
getTyConInScope s = do
  names <- GHC.parseName s
  tyThings <- Maybe.catMaybes <$> traverse GHC.lookupName names
  let tyCons = Maybe.mapMaybe tyThingTyCon tyThings
  pure (head tyCons)

-- Get an expression in the current scope, throw if not found
getExprInScope :: HasCallStack => String -> Ghc (LHsExpr GhcPs)
getExprInScope s = do
  names <- GHC.parseName s
  tyThings <- Maybe.catMaybes <$> traverse GHC.lookupName names
  let ids = map TyThing.tyThingId tyThings
  identToExpr (head ids)

-- construct the Identity monad type
getIdentityTyCon :: Ghc TyCon
getIdentityTyCon = do
  addImport "Data.Functor.Identity"
  getTyConInScope "Identity"

-- construct the runIdentity function
getUnwrapIdentity :: HasCallStack => Ghc (LHsExpr GhcPs)
getUnwrapIdentity = do
  addImport "Data.Functor.Identity"
  getExprInScope "runIdentity"

-- construct the IO monad type
getIOTyCon :: Ghc TyCon
getIOTyCon = getTyConInScope "IO"

findType :: GHC.TyCon -> [(GHC.TyCon, Type)] -> [Type]
findType tc l = do
  (x, y) <- l
  case Type.nonDetCmpTc x tc of
    EQ -> pure y
    _ -> []

getBindingIdsInScope :: Ghc [Id]
getBindingIdsInScope = do
  names <- filter Name.isValName <$> GHC.getNamesInScope
  tyThings <- Maybe.catMaybes <$> traverse GHC.lookupName names
  let ids = map TyThing.tyThingId tyThings
  pure ids

-- get inner monad from outer stack
getInnerMonad :: HasCallStack => Type -> Type
getInnerMonad stackType = snd (Type.splitAppTys (removeForAll stackType)) !! 1

lookupTyConSynonym :: TyCon -> TyCon
lookupTyConSynonym tyCon =
  case TyCon.synTyConRhs_maybe tyCon of
    Just ty -> lookupTyConSynonym (Maybe.fromJust (tyToTyCon ty))
    Nothing -> tyCon
module Synthesize.GHC where

import qualified Data.Maybe as Maybe
import GHC (LHsExpr, TyCon, Type)
import qualified GHC
import GHC.Core.TyCo.Rep (Type (..))
import GHC.Core.TyCon (synTyConRhs_maybe)
import qualified GHC.Core.TyCon as TyCon
import qualified GHC.Core.Type as Type
import GHC.Driver.Monad (GhcMonad)
import GHC.Hs (GhcPs)
import GHC.Stack (HasCallStack)
import GHC.Types.Id (Id)
import qualified GHC.Types.Name as Name
import GHC.Types.TyThing (TyThing (..))
import qualified GHC.Types.TyThing as TyThing
import qualified GHC.Types.Var as Var
import qualified GHC.Utils.Outputable as Outputable
import GHC.Core.TyCon
import GHC.Parser.Annotation as Annotation
import GHC.Types.SrcLoc as SrcLoc
import Language.Haskell.Syntax.Extension as Extension

-- Convenience data type for packaging parsed expressions and types
data TypedExpr = TypedExpr (LHsExpr GhcPs) Type

-- Import a package into an interactive session
addImport :: GhcMonad m => String -> m ()
addImport name = do
  let importedModule = GHC.IIDecl (GHC.simpleImportDecl (GHC.mkModuleName name))
  ctx <- GHC.getContext
  GHC.setContext (importedModule : ctx)

-- Try to convert a TyThing into a TyCon
tyThingTyCon :: TyThing -> Maybe TyCon
tyThingTyCon tyThing = case tyThing of
  ATyCon tyCon -> Just tyCon
  _ -> Nothing

-- Try to convert a Type into a TyCon
tyToTyCon :: Type -> Maybe TyCon
tyToTyCon ty = case ty of
  TyConApp tyCon _ -> Just tyCon
  _ -> Nothing

tyThingToTypedExpr :: GhcMonad m => TyThing -> m TypedExpr
tyThingToTypedExpr tyThing = do
  let ident = TyThing.tyThingId tyThing
  let ty = Var.varType ident
  expr <- identToExpr ident
  pure (TypedExpr expr ty)

-- Convert an Id into a parsed expression
identToExpr :: GhcMonad m => Id -> m (LHsExpr GhcPs)
identToExpr = GHC.parseExpr . show . Outputable.ppr . Var.varName

-- Recursively remove the for all type constructors from a Type
removeForAll :: Type -> Type
removeForAll t = case t of
  ForAllTy _ t1 -> removeForAll t1
  _ -> t

getArgType :: Type -> Maybe Type
getArgType ty = do
  (_, argType, _) <- Type.splitFunTy_maybe (removeForAll ty)
  pure argType

-- Get a TyCon in the current scope, throw if not found
getTyConInScope :: (HasCallStack, GhcMonad m) => String -> m TyCon
getTyConInScope s = do
  names <- GHC.parseName s
  tyThings <- Maybe.catMaybes <$> traverse GHC.lookupName names
  let tyCons = Maybe.mapMaybe tyThingTyCon tyThings
  pure (head tyCons)

-- Get an expression in the current scope, throw if not found
getTypedExprInScope :: (HasCallStack, GhcMonad m) => String -> m TypedExpr
getTypedExprInScope s = do
  names <- GHC.parseName s
  tyThings <- Maybe.catMaybes <$> traverse GHC.lookupName names
  typedExprs <- traverse tyThingToTypedExpr tyThings
  pure (head typedExprs)

-- Construct the Identity monad type
getIdentityTyCon :: GhcMonad m => m TyCon
getIdentityTyCon = do
  addImport "Data.Functor.Identity"
  getTyConInScope "Identity"

-- Construct the runIdentity function
getRunIdentityTypedExpr :: (HasCallStack, GhcMonad m) => m TypedExpr
getRunIdentityTypedExpr = do
  addImport "Data.Functor.Identity"
  getTypedExprInScope "runIdentity"

-- Construct the IO monad type
getIOTyCon :: GhcMonad m => m TyCon
getIOTyCon = getTyConInScope "IO"

-- Get all Ids of names in scope
getBindingIdsInScope :: GhcMonad m => m [Id]
getBindingIdsInScope = do
  names <- filter Name.isValName <$> GHC.getNamesInScope
  tyThings <- Maybe.catMaybes <$> traverse GHC.lookupName names
  let ids = map TyThing.tyThingId tyThings
  pure ids

-- Get inner monad from outer stack
getInnerMonad :: HasCallStack => Type -> Type
getInnerMonad stackType = snd (Type.splitAppTys (removeForAll stackType)) !! 1

-- Disambiguate a TyCon into its synonym, recursively
lookupTyConSynonym :: TyCon -> TyCon
lookupTyConSynonym tyCon =
  case TyCon.synTyConRhs_maybe tyCon of
    Just ty -> lookupTyConSynonym (Maybe.fromJust (tyToTyCon ty))
    Nothing -> tyCon

-- Get the arity of a funtion type, return 0 if not function
getArity :: Type -> Int
getArity = go . removeForAll
  where
    go ty = case ty of
      FunTy _ _ _ resultTy ->
        1 + getArity resultTy
      _ -> 0

getHoleExpr :: GhcMonad m => m (LHsExpr GhcPs)
getHoleExpr = GHC.parseExpr "_"

getFunc :: GhcMonad m => String -> m (LHsExpr GhcPs)
getFunc = GHC.parseExpr

extractArgument :: LHsExpr GhcPs -> LHsExpr GhcPs
extractArgument (SrcLoc.L y x) = case x of
  GHC.HsApp _ _ (SrcLoc.L c b) -> case b of
    GHC.HsApp _ a _ -> a
    _ -> SrcLoc.L c b
  _ -> SrcLoc.L y x

argA :: GhcMonad m => m (LHsExpr GhcPs)
argA = extractArgument <$> getFunc "unwords a"

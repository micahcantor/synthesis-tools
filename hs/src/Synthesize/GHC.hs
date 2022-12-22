{-# LANGUAGE ScopedTypeVariables #-}
module Synthesize.GHC where

import qualified Data.Maybe as Maybe
import GHC (LHsExpr, TyCon, TyThing (..), Type)
import qualified GHC
import GHC.Core.TyCo.Rep (Type (..))
import qualified GHC.Core.TyCon as TyCon
import GHC.Core.Type (TCvSubst (..))
import qualified GHC.Core.Type as Type
import qualified GHC.Core.Unify as Unify
import GHC.Driver.Monad (GhcMonad)
import GHC.Hs (GhcPs)
import GHC.Types.Id (Id)
import qualified GHC.Types.Name as Name
import GHC.Types.SrcLoc as SrcLoc (GenLocated (L))
import qualified GHC.Types.TyThing as TyThing
import qualified GHC.Types.Var as Var
import qualified GHC.Utils.Outputable as Outputable
import Debug.Trace ( traceShow, traceShowM )
import Data.Traversable (for)
import Control.Exception.Safe (try, SomeException)

-- Convenience data type for packaging parsed expressions and types
data TypedExpr = TypedExpr (LHsExpr GhcPs) Type

-- Import a package into an interactive session
addImport :: GhcMonad m => String -> m ()
addImport name = do
  let importedModule = GHC.IIDecl (GHC.simpleImportDecl (GHC.mkModuleName name))
  ctx <- GHC.getContext
  GHC.setContext (importedModule : ctx)

unifyTypeEq :: Type -> Type -> Bool
unifyTypeEq ty1 ty2 = Maybe.isJust (Unify.tcMatchTyKi ty1 ty2)

-- Try to compare a given type to the type of the first argument of a given function type
argTypeEq :: Type -> Type -> Bool
argTypeEq funTy ty =
  case getArgType funTy of
    Just funArgTy ->
      case Unify.tcMatchTyKi funArgTy (removeForAll ty) of
        Just (TCvSubst _ substEnv _) -> 
          traceShow (Outputable.ppr substEnv) True
        Nothing -> False
    Nothing -> False

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

tyThingToTypedExpr :: GhcMonad m => TyThing -> m (Maybe TypedExpr)
tyThingToTypedExpr tyThing = do
  let ident = TyThing.tyThingId tyThing
  let ty = Var.varType ident
  exprResult <- identToExpr ident
  case exprResult of
    Nothing -> pure Nothing
    Just expr ->  pure (Just (TypedExpr expr ty))

-- Try to convert an Id into a parsed expression
identToExpr :: GhcMonad m => Id -> m (Maybe (LHsExpr GhcPs))
identToExpr ident = do
  let identStr = show (Outputable.ppr (Var.varName ident))
  parseResult <- try (GHC.parseExpr identStr)
  case parseResult of
    Left (_ :: SomeException) -> pure Nothing
    Right expr -> pure (Just expr)

-- Recursively remove the for all type constructors from a Type
removeForAll :: Type -> Type
removeForAll t = case t of
  ForAllTy _ t1 -> removeForAll t1
  _ -> t

getArgType :: Type -> Maybe Type
getArgType ty = do
  (_, argType, _) <- Type.splitFunTy_maybe (removeForAll ty)
  pure argType

-- Try to get a TyCon in the current scope
getTyConInScope :: GhcMonad m => String -> m (Maybe TyCon)
getTyConInScope s = do
  names <- GHC.parseName s
  tyThings <- Maybe.catMaybes <$> traverse GHC.lookupName names
  let tyCons = Maybe.mapMaybe tyThingTyCon tyThings
  case tyCons of
    [] -> pure Nothing
    (tyCon : _) -> pure (Just tyCon)

-- Try to get an expression in the current scope
getTypedExprInScope :: GhcMonad m => String -> m (Maybe TypedExpr)
getTypedExprInScope s = do
  names <- GHC.parseName s
  tyThings <- Maybe.catMaybes <$> traverse GHC.lookupName names
  typedExprs <- Maybe.catMaybes <$> traverse tyThingToTypedExpr tyThings
  case typedExprs of
    [] -> pure Nothing
    (expr : _) -> pure (Just expr)

-- Construct the Identity monad type
getIdentityTyCon :: GhcMonad m => m TyCon
getIdentityTyCon = do
  addImport "Data.Functor.Identity"
  Maybe.fromJust <$> getTyConInScope "Identity"

-- Construct the runIdentity function
getRunIdentityTypedExpr :: GhcMonad m => m TypedExpr
getRunIdentityTypedExpr = do
  addImport "Data.Functor.Identity"
  Maybe.fromJust <$> getTypedExprInScope "runIdentity"

-- Construct the IO monad type
getIOTyCon :: GhcMonad m => m TyCon
getIOTyCon = Maybe.fromJust <$> getTyConInScope "IO"

-- Get all Ids of names in scope
getBindingIdsInScope :: GhcMonad m => m [Id]
getBindingIdsInScope = do
  names <- filter Name.isValName <$> GHC.getNamesInScope
  tyThings <- Maybe.catMaybes <$> traverse GHC.lookupName names
  let ids = map TyThing.tyThingId tyThings
  pure ids

getTypedExprsInScope :: GhcMonad m => m [TypedExpr]
getTypedExprsInScope = do
  idents <- getBindingIdsInScope
  let types = map Var.varType idents
  let unwrapperIdentTypes = [(ident, ty) | (ident, ty) <- zip idents types]
  unwrapperExprs <- for unwrapperIdentTypes $ \(ident, ty) -> do
    exprResult <- identToExpr ident
    case exprResult of
      Nothing -> pure Nothing
      Just expr -> pure (Just (TypedExpr expr ty))
  pure (Maybe.catMaybes unwrapperExprs)
    

-- Try to get inner monad from outer stack
getInnerMonad :: Type -> Maybe Type
getInnerMonad stackType =
  let (_, typeArgs) = Type.splitAppTys (removeForAll stackType)
   in case typeArgs of
        (_ : m : _) -> Just m
        _ -> Nothing

-- Disambiguate a TyCon into its synonym, recursively
lookupTyConSynonym :: TyCon -> TyCon
lookupTyConSynonym tyCon =
  case TyCon.synTyConRhs_maybe tyCon of
    Just ty -> maybe tyCon lookupTyConSynonym (tyToTyCon ty)
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

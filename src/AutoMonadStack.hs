{-# HLINT ignore "Eta reduce" #-}

module AutoMonadStack where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Maybe as Maybe
import GHC (Ghc, GhcPass, HValue, LHsExpr, Type)
import qualified GHC
import GHC.Core.TyCo.Rep (Type (..))
import GHC.Core.TyCon (TyCon(..))
import qualified GHC.Core.Type as Type
import GHC.Hs (GhcPs)
import qualified GHC.Hs.Utils as Utils
import GHC.Types.Id (Id)
import qualified GHC.Types.Name as Name
import qualified GHC.Types.TyThing as TyThing
import qualified GHC.Types.Var as Var
import qualified GHC.Utils.Outputable as Outputable
import GHC.Types.TyThing (TyThing(..))
import GHC.Stack (HasCallStack)

-- Import a package into an interactive session
addImport :: String -> Ghc ()
addImport name = do
  let importedModule = GHC.IIDecl (GHC.simpleImportDecl (GHC.mkModuleName name))
  ctx <- GHC.getContext
  GHC.setContext (importedModule : ctx)

-- get inner monad from outer stack
getInnerMonad :: Type -> Type
getInnerMonad stackType = snd (Type.splitAppTys (removeForAll stackType)) !! 1

getUnwrappingFunctionExpr :: Type -> Ghc (LHsExpr GhcPs)
getUnwrappingFunctionExpr stackType = do
  let stackTyCon = Maybe.fromJust $ tyToTyCon (removeForAll stackType)
  idents <- getBindingIdsInScope
  let types = map Var.varType idents
  let unwrapperIdents = [ident | (ident, ty) <- zip idents types, isUnwrappingType ty stackTyCon]
  liftIO (print (map Outputable.ppr unwrapperIdents))
  unwrapperExprs <- traverse identToExpr unwrapperIdents
  pure (head unwrapperExprs)
  where
    isUnwrappingType :: Type -> TyCon -> Bool
    isUnwrappingType ty tyCon =
      case Type.splitFunTy_maybe (removeForAll ty) of
        Just (_, argType, _) -> tyToTyCon argType == Just tyCon
        Nothing -> False

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

getTyConInScope :: HasCallStack => String -> Ghc TyCon
getTyConInScope s = do
  names <- GHC.parseName s
  tyThings <- Maybe.catMaybes <$> traverse GHC.lookupName names
  let tyCons = Maybe.mapMaybe tyThingTyCon tyThings
  pure (head tyCons)

-- construct the Identity monad type
getIdentityTyCon :: Ghc TyCon
getIdentityTyCon = do
  addImport "Data.Functor.Identity"
  getTyConInScope "Identity"

getUnwrapIdentity :: Ghc (LHsExpr GhcPs)
getUnwrapIdentity = undefined

-- construct the IO monad type
getIOTyCon :: Ghc TyCon
getIOTyCon = getTyConInScope "IO"

getBindingIdsInScope :: Ghc [Id]
getBindingIdsInScope = do
  names <- filter Name.isValName <$> GHC.getNamesInScope
  tyThings <- Maybe.catMaybes <$> traverse GHC.lookupName names
  let ids = map TyThing.tyThingId tyThings
  pure ids

synthesizeRunStack :: LHsExpr GhcPs -> Type -> Ghc HValue
synthesizeRunStack stackExpr stackType = do
  identityTyCon <- getIdentityTyCon
  ioTyCon <- getIOTyCon
  liftIO (print (Outputable.ppr ioTyCon, Outputable.ppr identityTyCon))
  unwrappers <- getUnwrappers stackType ioTyCon identityTyCon
  let app = foldr Utils.mkHsApp stackExpr unwrappers
  liftIO (print (Outputable.ppr app))
  GHC.compileParsedExpr app
  where
    -- recursively get unwrapping functions for the stack
    getUnwrappers :: Type -> TyCon -> TyCon -> Ghc [LHsExpr GhcPs]
    getUnwrappers stackType' ioTyCon identityTyCon
      | Just ioTyCon == tyToTyCon stackType' = pure []
      | Just identityTyCon == tyToTyCon stackType' = do
          unwrapIdentity <- getUnwrapIdentity
          pure [unwrapIdentity]
      | otherwise = do
          unwrapper <- getUnwrappingFunctionExpr stackType'
          innerUnwrappers <- getUnwrappers (getInnerMonad stackType') ioTyCon identityTyCon
          pure (unwrapper : innerUnwrappers)

makeRunStack :: String -> Ghc HValue
makeRunStack stackName = do
  stackExpr <- GHC.parseExpr stackName
  stackType <- GHC.exprType GHC.TM_Inst stackName
  liftIO (print (Outputable.ppr stackExpr, Outputable.ppr stackType))
  synthesizeRunStack stackExpr stackType
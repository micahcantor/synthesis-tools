{-# HLINT ignore "Eta reduce" #-}

module AutoMonadStack where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Maybe as Maybe
import GHC (Ghc, GhcPass, HValue, LHsExpr, Type)
import qualified GHC
import GHC.Core.TyCo.Rep (Type (..))
import qualified GHC.Core.Type as Type
import qualified GHC.Hs.Utils as Utils
import GHC.Types.Id (Id)
import qualified GHC.Types.Name as Name
import qualified GHC.Types.TyThing as TyThing
import qualified GHC.Types.Var as Var
import qualified GHC.Utils.Outputable as Outputable
import GHC.Hs (GhcPs)
import GHC.Core.TyCon (TyCon)

-- get inner monad from outer stack
getInnerMonad :: Type -> Type
getInnerMonad stackType = snd (Type.splitAppTys stackType) !! 1

getUnwrappingFunctionExpr :: Type -> Ghc (LHsExpr GhcPs)
getUnwrappingFunctionExpr stackType = do
  ids <- getBindingIdsInScope 
  let types = map Var.varType ids
  let unwrapperIdents = [ident | (ident, ty) <- zip ids types, isUnwrappingType stackType ty]
  unwrapperExprs <- mapM identToExpr unwrapperIdents
  pure (head unwrapperExprs)

identToExpr :: Id -> Ghc (LHsExpr GhcPs)
identToExpr = GHC.parseExpr . show  . Outputable.ppr  . Var.varName

removeForAll :: Type -> Type
removeForAll t = case t of
  ForAllTy _ t1 -> removeForAll t1
  _ -> t

isUnwrappingType :: Type -> Type -> Bool
isUnwrappingType stackType ty =
  case Type.splitFunTy_maybe (removeForAll ty) of
    Just (_, argType, _) -> Type.eqType argType stackType
    Nothing -> False

-- construct the Identity monad type
getIdentityType :: Ghc Type
getIdentityType = undefined

-- construct the IO monad type
getIOTyCon :: Ghc TyCon
getIOTyCon = undefined

getBindingIdsInScope :: Ghc [Id]
getBindingIdsInScope = do
  names <- filter Name.isValName <$> GHC.getNamesInScope
  tyThings <- Maybe.catMaybes <$> mapM GHC.lookupName names
  let ids = map TyThing.tyThingId tyThings
  pure ids

getTypesInScope :: Ghc [Type]
getTypesInScope = do
  bindings <- GHC.getBindings
  let bindingTypes = map (Var.varType . TyThing.tyThingId) bindings
  pure bindingTypes

synthesizeRunStack :: LHsExpr GhcPs -> Type -> Ghc HValue
synthesizeRunStack stackExpr stackType = do
  identityType <- getIdentityType
  ioTyCon <- getIOTyCon
  unwrappers <- getUnwrappers stackType ioType identityType
  let app = foldr Utils.mkHsApp stackExpr unwrappers
  GHC.compileParsedExpr app
  where
    -- recursively get unwrapping functions for the stack
    getUnwrappers :: Type -> Type -> Type -> Ghc [LHsExpr GhcPs]
    getUnwrappers stack ioType identityType
      | Type.eqType ioType stackType = pure []
      | Type.eqType identityType stack = do
          unwrapIdentity <- getUnwrappingFunctionExpr identityType
          pure [unwrapIdentity]
      | otherwise = do
          unwrapper <- getUnwrappingFunctionExpr stackType
          innerUnwrappers <- getUnwrappers (getInnerMonad stackType) ioType identityType
          pure $ unwrapper : innerUnwrappers

makeRunStack :: String -> [String] -> Ghc HValue
makeRunStack paramName functionNames = do
  paramExpr <- GHC.parseExpr paramName
  functionExprs <- mapM GHC.parseExpr functionNames
  let app = foldr Utils.mkHsApp paramExpr functionExprs
  liftIO (print (Outputable.ppr app))
  GHC.compileParsedExpr app
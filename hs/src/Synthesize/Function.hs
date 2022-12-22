module Synthesize.Function where

import qualified Data.Maybe as Maybe
import Data.Traversable (for)
import GHC (Type)
import qualified GHC.Core.Type as Type
import Synthesize.GHC (TypedExpr (..), getTypedExprsInScope, removeForAll, unifyTypeEq)
import Synthesize.Monad (SynthesizeM, SynthesisError (NoSolutionFound))
import qualified GHC.Utils.Outputable as Outputable
import Control.Monad.Except (throwError)

synthesizeFunction :: Type -> Type -> SynthesizeM String
synthesizeFunction actualType expectedType = do
  typedExprs <- getTypedExprsInScope
  splitFunResults <- for typedExprs $ \(TypedExpr expr ty) -> do
    case Type.splitFunTy_maybe (removeForAll ty) of
      Just (_, argType, resultType) ->
        if unifyTypeEq argType (removeForAll actualType)
          && unifyTypeEq resultType (removeForAll expectedType)
          then pure (Just expr)
          else pure Nothing
      Nothing -> pure Nothing
  let splitFuns = Maybe.catMaybes splitFunResults
  case splitFuns of
    (expr : _) -> pure (show (Outputable.ppr expr))
    _ -> throwError (NoSolutionFound (show (Outputable.ppr actualType)))

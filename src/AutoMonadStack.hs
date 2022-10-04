{-# HLINT ignore "Eta reduce" #-}

module AutoMonadStack where

import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC (HValue, Ghc)
import qualified GHC
import qualified GHC.Hs.Utils as Utils
import qualified GHC.Utils.Outputable as Outputable

-- runFunctions = [runExceptT, runIdentity]

{- idea:
  - use parseExpr on String names to get LHsExprs
  - give that mkHsApps
  - give that to compileParseExpr to get an HValue -}

{- To do:
  - Figure out how include extra parameters
  - Add searching for the correct function in scope with the type we want -}

makeRunStack :: String -> [String] -> Ghc HValue
makeRunStack paramName functionNames = do
  paramExpr <- GHC.parseExpr paramName
  functionExprs <- mapM GHC.parseExpr functionNames
  let app = foldr Utils.mkHsApp paramExpr functionExprs
  liftIO (print (Outputable.ppr app))
  GHC.compileParsedExpr app
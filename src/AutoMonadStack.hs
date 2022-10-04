{-# HLINT ignore "Eta reduce" #-}

module AutoMonadStack where

import GHC (HValue)
import qualified GHC
import qualified GHC.Hs.Utils as Utils
import qualified GHC.Paths

-- data Error
-- type StackM a = ExcepT Error Identity a

-- runEval :: StackM a -> Either Error a
-- runEval m = runExceptT (runIdentity m)

-- runFunctions = [runExceptT, runIdentity]

{- idea:
  - use parseExpr on String names to get LHsExprs
  - give that mkHsApps
  - give that to compileParseExpr to get an HValue -}

makeRunStack :: String -> [String] -> IO HValue
makeRunStack paramName functionNames = GHC.runGhc (Just GHC.Paths.libdir) $ do
  paramExpr <- GHC.parseExpr paramName
  functionExprs <- mapM GHC.parseExpr functionNames
  let app = Utils.mkHsApps paramExpr functionExprs
  GHC.compileParsedExpr app
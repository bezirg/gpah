module Hackage.Base where


import Control.DeepSeq
import Data.Monoid
import qualified Data.Map as M

data Analysis = Analysis {
      totalPackages :: Int
    , parsedModules :: Int
    , failedModules :: [String]
    , pkgsUsingGP :: Int
    , exesUsingGP :: Int
    , reverseDeps :: M.Map String [String]
    }
              deriving (Show)

instance NFData Analysis where
    rnf (Analysis a1 a2 a3 a4 a5 a6) = a1
                                   `deepseq` a2
                                   `deepseq` a3
                                   `deepseq` a4
                                   `deepseq` a5
                                   `deepseq` a6
                                   `deepseq` ()

instance Monoid Analysis where
    mempty = Analysis 0 0 [] 0 0 M.empty
    (Analysis x1 x2 x3 x4 x5 x6) `mappend` (Analysis y1 y2 y3 y4 y5 y6) = Analysis
                                                                          (x1+y1)
                                                                          (x2+y2)
                                                                          (x3++y3)
                                                                          (x4+y4)
                                                                          (x5+y5)
                                                                          (M.unionWith (++) x6 y6)

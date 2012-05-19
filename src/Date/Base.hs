module Date.Base where

import Control.DeepSeq
import Data.Monoid

import qualified Data.Map as M

data Analysis = Analysis {
                  yearUpdatesToSyb :: M.Map Int Int
                , yearUpdatesToUpl :: M.Map Int Int
                , yearNewToSyb :: M.Map Int Int
                , yearNewToUpl :: M.Map Int Int
                , hackageTime :: String
                }
              deriving (Show)

instance Monoid Analysis where
    mempty = Analysis M.empty M.empty M.empty M.empty mempty
    (Analysis x1 x2 x3 x4 x5) `mappend` (Analysis y1 y2 y3 y4 y5) = Analysis
                                                              (M.unionWith (+) x1 y1)
                                                              (M.unionWith (+) x2 y2)
                                                              (M.unionWith (+) x3 y3)
                                                              (M.unionWith (+) x4 y4)
                                                              (x5 ++ y5)



instance NFData Analysis where
    rnf (Analysis a1 a2 a3 a4 a5) = a1 
                                 `deepseq` a2
                                 `deepseq` a3
                                 `deepseq` a4
                                 `deepseq` a5
                                 `deepseq` ()


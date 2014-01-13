module Generics.GPAH.Date.Base where

import Control.DeepSeq
import Data.Monoid

import qualified Data.Map as M

data Analysis = Analysis {
                  yearUpdatesToSyb :: M.Map Int Int
                , yearUpdatesToUpl :: M.Map Int Int
                , yearNewToSyb :: M.Map Int Int
                , yearNewToUpl :: M.Map Int Int
                , hackageTime :: String
                , yearUpdatesToDerive :: M.Map Int Int
                , yearNewToDerive :: M.Map Int Int
                }
              deriving (Show)

instance Monoid Analysis where
    mempty = Analysis mempty mempty mempty mempty mempty mempty mempty
    (Analysis x1 x2 x3 x4 x5 x6 x7) `mappend` (Analysis y1 y2 y3 y4 y5 y6 y7) = Analysis
                                                              (M.unionWith (+) x1 y1)
                                                              (M.unionWith (+) x2 y2)
                                                              (M.unionWith (+) x3 y3)
                                                              (M.unionWith (+) x4 y4)
                                                              (x5 ++ y5)
                                                              (M.unionWith (+) x6 y6)
                                                              (M.unionWith (+) x7 y7)

instance NFData Analysis where
    rnf (Analysis a1 a2 a3 a4 a5 a6 a7) = a1 
                                 `deepseq` a2
                                 `deepseq` a3
                                 `deepseq` a4
                                 `deepseq` a5
                                 `deepseq` a6
                                 `deepseq` a7
                                 `deepseq` ()


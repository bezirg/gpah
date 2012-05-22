module Base where

import qualified Cpp.Base as Cpp
import qualified Deriving.Base as Deriving
import qualified Function.Base as Function
import qualified Upl.Base as Upl
import qualified Hackage.Base as Hackage
import qualified Date.Base as Date


import Data.Maybe

import Control.DeepSeq

import Data.Monoid

data Analysis = Analysis {
      cppAnalysis :: Cpp.Analysis
    , derAnalysis :: Deriving.Analysis
    , funAnalysis :: Function.Analysis
    , uplAnalysis :: Upl.Analysis
    , hacAnalysis :: Hackage.Analysis
    , dteAnalysis :: Date.Analysis
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
    mempty = Analysis mempty mempty mempty mempty mempty mempty
    (Analysis x1 x2 x3 x4 x5 x6) `mappend` (Analysis y1 y2 y3 y4 y5 y6) = Analysis
                                                                                (mappend x1 y1)
                                                                                (mappend x2 y2)
                                                                                (mappend x3 y3)
                                                                                (mappend x4 y4)
                                                                                (mappend x5 y5)
                                                                                (mappend x6 y6)


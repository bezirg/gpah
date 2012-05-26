module Derive.Base where

import qualified Data.Map as M
import Control.DeepSeq
import Data.Monoid

newtype Analysis = Analysis (M.Map String Int) -- derive directive strings
    deriving (Show)

instance Monoid Analysis where
    mempty = Analysis mempty
    (Analysis x) `mappend` (Analysis y) = Analysis $ M.unionWith (+) x y

instance NFData Analysis where
    rnf (Analysis x) = x `deepseq` ()

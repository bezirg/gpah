module Interp.Base where


import Control.DeepSeq
import Data.Monoid
import qualified Data.Map as M

newtype Analysis = Analysis (M.Map String [String]) -- PackageName to TypeSignatures
    deriving (Show)


instance Monoid Analysis where
    mempty = Analysis mempty
    (Analysis x) `mappend` (Analysis y) = Analysis $ M.unionWith (++) x y

instance NFData Analysis where
    rnf (Analysis x) = x `deepseq` ()

intTypes = sybTypes ++ uplTypes

sybTypes = ["GenericQ", "GenericT", "GenericM", "GenericB", "GenericR", "Generic'", "Generic ", "GenericT'", "GenericQ'", "GenericM'" ] 

uplTypes = ["Uniplate", "Biplate"]

excludePkgs = ["DSH", "freesect"] -- these pkgs contain modules that cannot be interpreted
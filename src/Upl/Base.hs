module Upl.Base where

import Utils
import qualified Data.Map as M
import Control.DeepSeq
import Data.Monoid

manualSpeed = ["Data.Generics.Uniplate.Direct",  
               "Data.Generics.PlateDirect"]
automatic = ["Data.Generics.Uniplate.Data",              
             "Data.Generics.PlateData"]
manualPointless = ["Data.Generics.Uniplate.Typeable",            
                   "Data.Generics.PlateTypeable"]
mixed = "Data.Generics.Uniplate.DataOnly"

data UniStyle = ManualSpeed | Automatic | ManualPointless | Mixed
              deriving (Read, Show, Eq, Ord)

instance NFData UniStyle        -- enumeration type, so default instance

newtype Analysis = Analysis (M.Map UniStyle [ModuleFileName])
    deriving (Show)

instance NFData Analysis where
    rnf (Analysis x) = x `deepseq` ()

instance Monoid Analysis where
    mempty = Analysis (mempty)
    (Analysis x) `mappend` (Analysis y) = Analysis $ M.unionWith (++) x y


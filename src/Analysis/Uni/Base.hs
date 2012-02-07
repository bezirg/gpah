module Analysis.Uni.Base where

import Analysis.Utils
import qualified Data.Map as M

manualSpeed = ["Data.Generics.Uniplate.Direct",  
               "Data.Generics.PlateDirect"]
automatic = ["Data.Generics.Uniplate.Data",              
             "Data.Generics.PlateData"]
manualPointless = ["Data.Generics.Uniplate.Typeable",            
                   "Data.Generics.PlateTypeable"]
mixed = "Data.Generics.Uniplate.DataOnly"

data UniStyle = ManualSpeed | Automatic | ManualPointless | Mixed
              deriving (Read, Show, Eq, Ord)

type Analysis = M.Map UniStyle [ModuleFileName]



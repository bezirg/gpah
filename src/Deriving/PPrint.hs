module Deriving.PPrint where

import Deriving.Base
import Text.CSV
import System.IO
import qualified Data.Map as M
import Data.Function
import Data.List

pprint :: Analysis -> FilePath -> IO ()
pprint (Analysis a1 a2 a3 a4 a5 a6) fp = do
  let p = [["NumberOfDerivingStatements", show a1],
           ["TopDerivedClasses", show $ sortBy (flip compare `on` snd) $ M.toList a2],
           ["NormalDeriving", show $ a3],
           ["StandaloneDeriving", show $ a4],
           ["NewtypeDeriving", show $ a5],
           ["InstancesPreferredToWriteManualThanAutoDerive", show $ a6]
          ]
      pCSV = printCSV p

  writeFile fp pCSV

  putStrLn "Deriving Results:\n###############"
  putStrLn pCSV

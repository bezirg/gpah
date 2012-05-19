module Upl.PPrint where

import Upl.Base
import Text.CSV
import System.IO
import qualified Data.Map as M
import Data.Function

pprint :: Analysis -> FilePath -> IO ()
pprint (Analysis m) fp = do
  let p = [["ModulesUsingManualSpeed(Direct)", show $ maybe 0 length $ M.lookup ManualSpeed m],
           ["ModulesUsingManualPointless(PlateTypeable)", show $ maybe 0 length $ M.lookup ManualPointless m],
           ["ModulesUsingAutomatic(PlateData)", show $ maybe 0 length $ M.lookup Automatic m],
           ["ModulesUsingMixed(DataOnly)", show $ maybe 0 length $ M.lookup Mixed m]
          ]
      pCSV = printCSV p

  writeFile fp pCSV

  putStrLn "Uniplate Results:\n###############"
  putStrLn pCSV


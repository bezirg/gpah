module Cpp.PPrint where

import Cpp.Base
import Text.CSV
import System.IO


pprint :: Analysis -> FilePath -> IO ()
pprint x fp = do
  let p = [["FailedToCppModules", show $ length x]
          ]
      pCSV = printCSV p

  writeFile fp pCSV

  putStrLn "Cpp Results:\n###############"
  putStrLn pCSV

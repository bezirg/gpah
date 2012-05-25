module Derive.PPrint where

import Derive.Base
import Text.CSV
import System.IO

pprint :: Analysis -> FilePath -> IO ()
pprint a1 fp = do
  let p = [["NumberOfDirectives", show $ length a1]
          ,["Directives", show $ a1]
          ]
      pCSV = printCSV p

  writeFile fp pCSV

  putStrLn "Derive Results:\n###############"
  putStrLn pCSV

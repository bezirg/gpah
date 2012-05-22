module Date.PPrint where

import Date.Base
import System.IO
import qualified Data.Map as M
import Text.CSV
    
pprint :: Analysis -> FilePath -> IO ()
pprint (Analysis a1 a2 a3 a4 a5) fp = do
  let p = [["HackageArchiveCloneTime", a5],
           ["YearlyUpdatesToPackagesUsingSyb", show $ M.toList a1],
           ["YearlyUpdatesToPackagesUsingUniplate", show $ M.toList a2],
           ["YearlyNewPackagesUsingSyb", show $ M.toList a3],
           ["YearlyNewPackagesUsingUniplate", show $ M.toList a4]
          ]
      pCSV = printCSV p

  writeFile fp pCSV

  putStrLn "Date Results:\n###############"
  putStrLn pCSV
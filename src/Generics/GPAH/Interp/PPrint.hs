module Generics.GPAH.Interp.PPrint where

import Generics.GPAH.Interp.Base
import Text.CSV
import System.IO
import qualified Data.Map as M
import Data.List
import Data.Function (on)


pprint :: Analysis -> FilePath -> IO ()
pprint (Analysis a1) fp = do
  let p = [["NrTypes", show $ length $ concat $ M.elems a1],
           ["AssocTypes", show $ map (\ t -> (t, length $ filter (t `isInfixOf`) $ concat $ M.elems a1))  intTypes]
          ]
      pCSV = printCSV p

  writeFile fp pCSV

  putStrLn "Interpretation Results:\n###############"
  putStrLn pCSV


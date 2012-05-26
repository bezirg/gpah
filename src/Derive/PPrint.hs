module Derive.PPrint where

import Derive.Base
import Text.CSV
import System.IO
import qualified Data.Map as M
import Data.Function
import Data.List

pprint :: Analysis -> FilePath -> IO ()
pprint (Analysis a1) fp = do
  let p = [["NumberOfDirectives", show $ M.foldl (+) 0 a1]
          ,["TopDirectives", show $ sortBy (flip compare `on` snd) $ M.toList a1]
          ]
      pCSV = printCSV p

  writeFile fp pCSV

  putStrLn "Derive Results:\n###############"
  putStrLn pCSV

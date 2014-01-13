module Generics.GPAH.Derive.PPrint where

import Generics.GPAH.Derive.Base
import Text.CSV
import System.IO
import qualified Data.Map as M
import Data.Function
import Data.List

pprint :: Analysis -> FilePath -> IO ()
pprint (Analysis a1 a2 a3 a4) fp = do
  let p = [["DeriveTemplateHaskellDirectives", show $ sortBy (flip compare `on` snd) $ M.toList a1]
          ,["DerivePreprocessingDirectives",  show $ sortBy (flip compare `on` snd) $ M.toList a2]
          ,["DrIFTNormalDirectives",  show $ sortBy (flip compare `on` snd) $ M.toList a3]
          ,["DrIFTGlobalDirectives",  show $ sortBy (flip compare `on` snd) $ M.toList a4]                                                                    
          ]
      pCSV = printCSV p

  writeFile fp pCSV

  putStrLn "Derive+DrIFT Results:\n###############"
  putStrLn pCSV

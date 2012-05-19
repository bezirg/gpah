module Hackage.PPrint where

import Hackage.Base
import Text.CSV
import System.IO
import qualified Data.Map as M
import Data.List
import Data.Function (on)


pprint :: Analysis -> FilePath -> IO ()
pprint (Analysis a1 a2 a3 a4 a5 a6) fp = do
  let p = [["TotalNumberOfPackagesInHackage", show a1],
           ["SuccessfullyParsedModules", show a2],
           ["FailedToParseModules", show $ length a3],
           ["PackagesUsingGP", show a4],
           ["PackagesUsingGPwithAnExe", show a5],
           ["PackagesUsingSyb", show $ maybe 0 length $ M.lookup "syb" a6],
           ["PackagesUsingUniplate", show $ maybe 0 length $ M.lookup "uniplate" a6],
           ["ReverseDeps", show $ sortBy (flip compare `on` snd) $ M.toList $ M.map length a6]
          ]
      pCSV = printCSV p

  writeFile fp pCSV

  putStrLn "Hackage Results:\n###############"
  putStrLn pCSV


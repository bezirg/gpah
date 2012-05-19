module Function.PPrint where

import Function.Base
import Text.CSV
import System.IO
import qualified Data.Map as M
import Data.Function
import Data.List

import System.IO

pprint :: Analysis -> FilePath -> IO ()
pprint (Analysis m) fp = do
  let p = [["TotalFunctionOccurences", show $ M.foldl (+) 0 m],
           ["TopFunctions", show $ sortBy (flip compare `on` snd) $ M.toList m],
           ["Transformation/QueryRatio", show $ countT m / countQ m]
          ]
      pCSV = printCSV p

  writeFile fp pCSV

  putStrLn "Function Results:\n###############"
  putStrLn pCSV


countT :: M.Map FavFunction Int -> Double
countT m = count m tList
countQ :: M.Map FavFunction Int -> Double
countQ m = count m qList
-- | the list of transformation-functions to search for                     
tList = ["mkT","mkM","gmapT","everywhere", "everywhereM", "transform", "transformBi"]
-- | the list of query-functions to search for
qList = ["mkQ","gmapQ","everything", "universe", "universeBi"]

count m fs = toEnum $ foldl (\ acc f -> acc + M.findWithDefault 0 f m) 0 fs

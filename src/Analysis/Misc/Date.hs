module Main where

import Analysis.Utils

import Data.List
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.Function (on)


yearUpdatesToLib :: [String] -> [(Int, String)] -> M.Map Int Int
yearUpdatesToLib favLibs cLog = foldl (\ acc (year, pkg) -> if pkg `elem` favLibs 
                                                           then M.insertWith (+) year 1 acc
                                                           else acc) 
                                M.empty cLog
yearNewToLib favLibs cLog = yearUpdatesToLib favLibs (nubBy ((==) `on` snd) cLog)


-- The updates to SYB-dependent libraries per year
yearUpdatesToSyb = yearUpdatesToLib libsUsingSyb
-- The updates to Uniplate-dependent libraries per year
yearUpdatesToUni = yearUpdatesToLib libsUsingUni
-- Newcoming SYB-dependent libraries per year
yearNewToSyb = yearNewToLib libsUsingSyb
-- Newcoming Uniplate-dependent libraries per year
yearNewToUni = yearNewToLib libsUsingUni


cleanLog :: [[String]] -> [(Int, String)]
cleanLog = map cleanLog' where
  --cleanLog' [String] -> (PackageName, Year, Month)
  cleanLog' [_, _, _, _, _, y, _, pkg, _]  = (read y :: Int, pkg)
                                      
                                             
main = do
  contents <- readFile hackageLog
  let log = map words $ lines contents :: [[String]]
  let cLog = cleanLog log
  putStrLn $ "Updates to Syb: "  ++ show (yearUpdatesToSyb cLog)
  putStrLn $ "Updates to Uni: "  ++ show (yearUpdatesToUni cLog)
  putStrLn $ "New to Syb: "      ++ show (yearNewToSyb cLog)
  putStrLn $ "New to Uni: "      ++ show (yearNewToUni cLog)
                                              
                                             
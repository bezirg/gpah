module Date.Analyze where

import Utils
import Date.Base
import qualified Base
import qualified Hackage.Base as Hackage

import Data.List
import qualified Data.Map as M
import Data.Function (on)
import Data.Monoid


yearUpdatesToLib :: [String] -> [(Int, String)] -> M.Map Int Int
yearUpdatesToLib favLibs cLog = foldl (\ acc (year, pkg) -> if pkg `elem` favLibs 
                                                           then M.insertWith (+) year 1 acc
                                                           else acc) 
                                M.empty cLog
yearNewToLib favLibs cLog = yearUpdatesToLib favLibs (nubBy ((==) `on` snd) cLog)


cleanLog :: [[String]] -> [(Int, String)]
cleanLog = map cleanLog' where
  --cleanLog' [String] -> (PackageName, Year, Month)
  cleanLog' [_, _, _, _, _, y, _, pkg, _]  = (read y :: Int, pkg)
                                      

analyzeHackageLog :: String -> Base.Analysis -> Base.Analysis
analyzeHackageLog contents a =   let hacTime = unwords $ take 6 $ words $ last $ lines contents
                                     log = map words $ lines contents :: [[String]]
                                     cLog = cleanLog log
                                     pus = M.findWithDefault [] "syb" $ (Hackage.reverseDeps . Base.hacAnalysis) a -- packages that use syb
                                     puu = M.findWithDefault [] "uniplate" $ (Hackage.reverseDeps . Base.hacAnalysis) a -- packages that use uniplate
                                 -- construct a new base analysis and adds the date analysis
                                 in mempty {Base.dteAnalysis = Analysis (yearUpdatesToLib pus cLog) (yearUpdatesToLib puu cLog) (yearNewToLib pus cLog) (yearNewToLib puu cLog) hacTime}


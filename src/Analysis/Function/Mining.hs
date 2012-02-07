module Main where

import Analysis.Function.Base

import qualified Data.Map as M
import qualified Data.List as L

-- | gives the functions in order of most nr of occurences to least nr of occurences 
topFunctions :: Analysis -> [(String,Int)]
topFunctions res = L.sortBy eqTups $ L.sort $ M.toList $ M.map (foldr (+) 0) $ M.map M.elems res

-- | gives the transformation/query ratio in the form of a tuple (nr of trans-functions, nr of query-functions)
transToQueryRatio :: Analysis -> (Int,Int)
transToQueryRatio res = let
  top = topFunctions res
  ratio = rat tList qList top
  in ratio

-- | gives the transformation/query ratio based on two inputlists for transformation functions 
-- | and query functions and the list of top functions
rat :: [String] -> [String] -> [(String,Int)] -> (Int,Int)
rat _ _ []         = (0,0)
rat t q ((s,i):xs) = if s `L.elem` t then (i + t',q') else 
                     if s `L.elem` q then (t',i + q') else (t',q')
                     where 
                     (t',q') = rat t q xs

-- | the list of transformation-functions to search for                     
tList = ["mkT","mkM","gmapT","everywhere", "everywhereM", "transform", "transformBi"]
-- | the list of query-functions to search for
qList = ["mkQ","gmapQ","everything", "universe", "universeBi"]

-- | defines an ordering on pairs of Strings and Ints
eqTups :: (String,Int) -> (String,Int) -> Ordering
eqTups (a,b) (a',b') | b > b'  = LT
                     | b < b'  = GT
                     | b == b' = EQ

-- | counts the totall number of function-occurences in the analysis                                
countOccs :: Analysis -> Int
countOccs = M.foldl (\ acc m -> M.foldl (+) acc m) 0

main = do                                
  contents <- getContents
  let resF = read contents :: Analysis

  putStrLn $ "Total function occurrences: " ++ show (countOccs resF)
  putStrLn $ "Top functions: " ++ show (topFunctions resF)
  putStrLn $ "Transformation / Query ratio: " ++ show (transToQueryRatio resF)

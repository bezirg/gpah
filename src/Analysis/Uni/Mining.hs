module Main where

import Analysis.Uni.Base
import Analysis.Utils
import qualified Data.Map as M
import Data.Maybe

main = do
  contents <- getContents
  let resUni = read contents :: Analysis

  putStrLn $ "Using ManualSpeed (Direct): " ++ show (length $ maybe [] id $ M.lookup ManualSpeed resUni)
  putStrLn $ "Using ManualPointless (PlateTypeable): " ++ show (length $ maybe [] id $ M.lookup ManualPointless resUni)
  putStrLn $ "Using Automatic (PlateData): " ++ show (length $ maybe [] id $ M.lookup Automatic resUni)
  putStrLn $ "Using Mixed (DataOnly): " ++ show (length $ maybe [] id $ M.lookup Mixed resUni)
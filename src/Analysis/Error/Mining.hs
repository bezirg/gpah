module Main where

import Analysis.Error.Base

main = do
  contents <- getContents
  let resE = read contents :: Analysis

  putStrLn $ "Modules failing to parse: " ++ show (length resE)
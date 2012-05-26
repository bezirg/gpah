module Derive.Analyze where

import Derive.Base
import qualified Hackage.Base as Hackage
import Text.Regex.PCRE
import Text.Regex.PCRE.String
import qualified Data.Map as M
import Data.Monoid
import Data.List
import Data.List.Utils (countElem)
import Data.String.Utils (split, strip)

analyzeModule :: FilePath -> Hackage.Analysis -> IO Analysis 
analyzeModule hs hacPkg | "derive" `elem` (M.keys (Hackage.reverseDeps hacPkg)) = do
  Right rx <- compile (compMultiline + compDotAll + compUngreedy) execBlank "\\$\\(\\s*derive(.*)\\)"
  inp <- readFile hs
  let 
      process :: String -> [(String, Int)]
      process s = let s' = strip s
                  in if (head s') == '['
                     then map (\ x -> (strip x, 1)) $ split "," $ takeWhile (/= ']') (tail s')
                     else [(strip (takeWhile (/= '\'') s'), 1)]
  return $ Analysis $ M.fromListWith (+) $ concatMap (process . head . tail) (match rx inp :: [[String]])
                        | otherwise = return mempty


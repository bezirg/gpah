module Derive.Analyze where

import Derive.Base
import qualified Hackage.Base as Hackage
import Text.Regex.PCRE
import Text.Regex.PCRE.String
import qualified Data.Map as M
import Data.Monoid

analyzeModule :: FilePath -> Hackage.Analysis -> IO Analysis 
analyzeModule hs hacPkg | "derive" `elem` (M.keys (Hackage.reverseDeps hacPkg)) = do
  Right rx <- compile (compMultiline + compDotAll + compUngreedy) execBlank "\\{-!.*!-\\}|\\$\\(\\s*derive.*\\)"  
  inp <- readFile hs
  return $ map head (match rx inp :: [[String]])
                        | otherwise = return mempty


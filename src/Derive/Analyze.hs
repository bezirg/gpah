{-# LANGUAGE FlexibleContexts #-}
module Derive.Analyze where

import Derive.Base
import qualified Hackage.Base as Hackage
import qualified Data.Map as M
import Data.Monoid
import Language.Haskell.Exts
import Language.Haskell.Exts.Comments
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils

import Generics.SYB


analyzeModule :: ParseResult (Module, [Comment]) -> Hackage.Analysis -> Analysis 
analyzeModule (ParseOk (m, c)) hacPkg = Analysis
                                        (runDeriveTH m hacPkg)
                                        (runDerivePP c)
                                        (runDriftPP c)
                                        (runDriftGL c)
analyzeModule _ _ = mempty

runDeriveTH :: Module -> Hackage.Analysis -> M.Map String Int
runDeriveTH m hacPkg | "derive" `elem` (M.keys (Hackage.reverseDeps hacPkg)) = everything (M.unionWith (+)) (mempty `mkQ` thDecl) m
    where
      thDecl :: Splice -> M.Map String Int
      thDecl (ParenSplice (App (App (Var (UnQual (Ident "derive"))) (Var (UnQual (Ident class_)))) (TypQuote (UnQual (Ident dt))))) = M.singleton class_ 1
      thDecl (ParenSplice (App (App (Var (UnQual (Ident "derive"))) (Var (UnQual (Ident class_)))) (List dts))) = M.singleton class_ (length dts)
      thDecl (ParenSplice (App (App (Var (UnQual (Ident "derive"))) (List classes_ids)) (TypQuote (UnQual (Ident dt))))) = let classes = map (\ (Var (UnQual (Ident class_))) -> class_) classes_ids
                                                                                                                           in
                                                                                                                             M.fromList (zip classes (repeat 1))
      thDecl (ParenSplice (App (App (Var (UnQual (Ident "derive"))) (List classes_ids)) (List dts))) = let classes = map (\ (Var (UnQual (Ident class_))) -> class_) classes_ids
                                                                                              in
                                                                                                M.fromList (zip classes (repeat (length dts)))
      thDecl _ = mempty
runDeriveTH _ _ = mempty


runDerivePP :: [Comment] -> M.Map String Int
runDerivePP cs = foldl (\ m r -> M.insertWith (+) r 1 m) mempty $ concatMap (\ (Comment ml _ s) -> case (ml, execParser pDerivePP s) of
                                                                                                      (True, (res, [])) -> res
                                                                                                      _ -> [] -- error in parsing or single-line
                                                                               ) cs

runDriftPP :: [Comment] -> M.Map String Int
runDriftPP cs = foldl (\ m r -> M.insertWith (+) r 1 m) mempty $ concatMap (\ (Comment _ _ s) -> case execParser pDriftPP s of
                                                                                                      (res, []) -> res
                                                                                                      _ -> [] -- error in parsing
                                                                               ) cs


runDriftGL :: [Comment] -> M.Map String Int
runDriftGL cs =foldl (\ m r -> M.insertWith (+) r 1 m) mempty $ concatMap (\ (Comment _ _ s) -> case execParser pDriftGL s of
                                                                                                      (res, []) -> res
                                                                                                      _ -> [] -- error in parsing
                                                                               ) cs


pDerivePP :: Parser [String]
pDerivePP = deriveStandalone <|> deriveInline
    where
      deriveInline :: Parser [String]
      deriveInline = pSym '!' *> pList1Sep (pSym ',') pcl <* pSpaces <* pSym '!'
          where
            pcl :: Parser String
            pcl =foldl1 (<|>)  (map (\ c -> pSpaces *> pToken c) deriveClasses)
      deriveStandalone :: Parser [String]
      deriveStandalone = pSym '!' *> pSpaces *> pToken "deriving" *> pSpaces *> pToken "instance" *> pList1Sep (pSym ',') pcl <* pSpaces <* pSym '!'
          where
            pcl :: Parser String
            pcl =foldl1 (<|>)  (map (\ c -> pSpaces *> pToken c) deriveClasses)

pDriftPP :: Parser [String]
pDriftPP = driftInline <|> driftStandalone
    where
      driftInline :: Parser [String]
      driftInline = pSym '!' *> pSpaces *> pToken "derive:" *> pList1Sep (pSym ',') pcl <* pSpaces <* pSym '!'
          where
            pcl :: Parser String
            pcl =foldl1 (<|>)  (map (\ c -> pSpaces *> pToken c) driftClasses)
      driftStandalone :: Parser [String]
      driftStandalone = pSym '!' *> pSpaces *> pToken "for" *> pSpaces *> pMunch (`notElem` " \n\r") *> pSpaces *> pToken "derive:" *> pList1Sep (pSym ',') pcl <* pSpaces <* pSym '!'
          where
            pcl :: Parser String
            pcl =foldl1 (<|>)  (map (\ c -> pSpaces *> pToken c) driftClasses)

pDriftGL :: Parser [String]
pDriftGL = driftGlobal
    where
      driftGlobal :: Parser [String]
      driftGlobal = pSym '!' *> pSpaces *> pToken "global:" *> pList1Sep (pSym ',') pcl <* pSpaces <* pSym '!'
          where
            pcl :: Parser String
            pcl =foldl1 (<|>)  (map (\ c -> pSpaces *> pToken c) driftClasses)


module Derive.Analyze where

import Derive.Base
import qualified Hackage.Base as Hackage
import qualified Data.Map as M
import Data.Monoid
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Generics.SYB


analyzeModule :: ParseResult Module -> Hackage.Analysis -> Analysis 
analyzeModule (ParseOk m) hacPkg | "derive" `elem` (M.keys (Hackage.reverseDeps hacPkg)) = everything (mappend) (mempty `mkQ` thDecl) m
                                 | otherwise = mempty
analyzeModule _ _ = mempty

thDecl :: Splice -> Analysis
thDecl (ParenSplice (App (App (Var (UnQual (Ident "derive"))) (Var (UnQual (Ident class_)))) (TypQuote (UnQual (Ident dt))))) = Analysis $ M.singleton class_ 1
thDecl (ParenSplice (App (App (Var (UnQual (Ident "derive"))) (Var (UnQual (Ident class_)))) (List dts))) = Analysis $ M.singleton class_ (length dts)
thDecl (ParenSplice (App (App (Var (UnQual (Ident "derive"))) (List classes_ids)) (TypQuote (UnQual (Ident dt))))) = let classes = map (\ (Var (UnQual (Ident class_))) -> class_) classes_ids
                                                                                                                    in
                                                                                                                      Analysis $ M.fromList (zip classes (repeat 1))
thDecl (ParenSplice (App (App (Var (UnQual (Ident "derive"))) (List classes_ids)) (List dts))) = let classes = map (\ (Var (UnQual (Ident class_))) -> class_) classes_ids
                                                                                              in
                                                                                                Analysis $ M.fromList (zip classes (repeat (length dts)))
thDecl _ = mempty

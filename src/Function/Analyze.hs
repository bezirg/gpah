module Function.Analyze where

import Function.Base
import Utils
import Language.Haskell.Exts
import Language.Haskell.Exts.Comments
import Hackage.Analyze

-- Cabal-related imports
import Distribution.PackageDescription hiding (Var)

import Data.Generics
import qualified Data.List as L
import qualified Data.Map as M

import Data.Monoid

-- | Constructs a list of keywords to search for
constructSearchList :: M.Map String [String] -> [String]
constructSearchList m = foldr (++) [] $ M.elems m

-- | if the given variable is the given function to search for a 1 is returned else a 0
findN :: FavFunction -> Exp -> Int
findN n e@(Var n') = case n' of 
                       UnQual i -> case i of
                                     Ident a  -> if a == n then 1 else 0
                                     Symbol a -> if a == n then 1 else 0
                       Qual _ i -> case i of
                                     Ident a  -> if a == n then 1 else 0
                                     Symbol a -> if a == n then 1 else 0
                       _        -> 0
findN _ _          = 0                                 

-- | collects for a given module the number of occurences of a given function
collectOccs :: Module -> FavFunction -> Int
collectOccs m n = everything (+) (0 `mkQ` findN n) m

-- | executes the function analysis on a given module
analyzeModule :: ParseResult (Module, [Comment]) -> GenericPackageDescription -> Analysis
analyzeModule (ParseOk (modul@(Module sloc _ _ _ _ _ _),_)) gpkgdesc =   
    let toSearch = L.nub $ constructSearchList {-(getImportNames modul)-} favorites
        ds = cabalDepends gpkgdesc
    in if "uniplate" `elem` ds || "syb" `elem` ds -- if uniplate or syb as dep
       then Analysis $ M.fromList $ filter (\(_,nr) -> nr > 0) $ map (\x -> (x, collectOccs modul x)) toSearch
       else mempty

analyzeModule _ _ = mempty

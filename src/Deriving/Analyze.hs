{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, ScopedTypeVariables #-}
module Deriving.Analyze where

import Deriving.Base
import Utils
import Language.Haskell.Exts
import qualified Data.Map as M

import Data.List

import Data.Monoid

analyzeModule :: ParseResult Module -> Analysis
analyzeModule (ParseOk (Module _ _ _ _ _ _ decls)) = foldr (\ decl acc -> analyzeDecl decl `mappend` acc) mempty decls
analyzeModule _ = mempty

analyzeDecl :: Decl -> Analysis

-- data or newtype
analyzeDecl (DataDecl _ dataOrNew _ _ name _ ds) = let lds = length ds 
                                                       nds = map derivingName ds
                                                   in 
                                                     Analysis  
                                                     lds -- size
                                                     (M.fromList $ zip nds (repeat 1)) -- top
                                                     lds -- normal
                                                     0   -- standalone
                                                     (if dataOrNew == NewType then length (favInstances `intersect` nds) else 0) -- newtype
                                                     0 -- overload


-- gadt data or new type
analyzeDecl (GDataDecl _ dataOrNew _ name _ _ _ ds) = analyzeDecl (DataDecl undefined dataOrNew undefined undefined undefined undefined ds) -- same as DataDecl

-- data or newtype families
analyzeDecl (DataInsDecl _ dataOrNew _  _ ds) = analyzeDecl (DataDecl undefined dataOrNew undefined undefined undefined undefined ds) -- same as DataDecl

-- gadt data or newtype families
analyzeDecl (GDataInsDecl _ dataOrNew typeParams _ _ ds) = analyzeDecl (DataDecl undefined dataOrNew undefined undefined undefined undefined ds) -- same as DataDecl

-- standalone deriving
analyzeDecl (DerivDecl _ _ derClass typeParams) = Analysis 
                                                  1 -- size
                                                  (M.singleton (qNameToString derClass) 1) -- top
                                                  0 -- normal
                                                  1 -- standalone
                                                  (fromBool $ (qNameToString derClass) `elem` favInstances) -- newtype
                                                  0 -- overload

analyzeDecl (InstDecl _ _ derClass typeParams _) = Analysis 0 M.empty 0 0 0 $ fromBool $ (qNameToString derClass) `elem` favInstances -- overload

analyzeDecl _ = mempty

{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, ScopedTypeVariables #-}
module Generics.GPAH.Deriving.Analyze where

import Generics.GPAH.Deriving.Base
import Generics.GPAH.Utils
import Language.Haskell.Exts
import Language.Haskell.Exts.Comments
import qualified Data.Map as M

import Data.List

import Data.Monoid

analyzeModule :: FilePath -> ParseResult (Module, [Comment]) -> Analysis
analyzeModule hs (ParseOk ((Module _ _ _ _ _ _ decls),_)) = foldr (\ decl acc -> analyzeDecl hs decl `mappend` acc) mempty decls
analyzeModule _ _ = mempty


analyzeDecl :: FilePath -> Decl -> Analysis

-- NORMAL
---------

-- data
analyzeDecl _ (DataDecl _ DataType _ _ name _ ds) = let lds = length ds 
                                                        nds = map derivingName ds
                                                    in mempty {
                                                            sizeStruct = 1
                                                          , normalStruct = 1
                                                          , sizePos = (fromBool $ not $ null ds)
                                                          , normalPos = (fromBool $ not $ null ds)
                                                          , sizeDer = lds
                                                          , normalDer = lds
                                                          , topDer = (M.fromList $ zip nds (repeat 1))
                                                          }
-- ok

-- newtype
analyzeDecl _ (DataDecl _ NewType _ _ name _ ds) = let lds = length ds 
                                                       nds = map derivingName ds
                                                   in mempty {
                                                            sizeStruct = 1
                                                          , normalStruct = 1
                                                          , newtypeStructContains = 1
                                                          , sizePos = (fromBool $ not $ null ds)
                                                          , normalPos = (fromBool $ not $ null ds)
                                                          , newtypePosContains = (fromBool $ not $ null ds)
                                                          , sizeDer = lds
                                                          , normalDer = lds
                                                          , newtypeDerContains = lds
                                                          }

-- ok

-- GADT
-------

-- data
analyzeDecl _ (GDataDecl _ DataType _ name _ _ _ ds) = let lds = length ds 
                                                           nds = map derivingName ds
                                                       in mempty {
                                                            sizeStruct = 1
                                                          , gadtStruct = 1
                                                          , sizePos = (fromBool $ not $ null ds)
                                                          , gadtPos = (fromBool $ not $ null ds)
                                                          , sizeDer = lds
                                                          , gadtDer = lds
                                                          , topDer = (M.fromList $ zip nds (repeat 1))
                                                          }

-- ok

-- newtype
analyzeDecl _ (GDataDecl _ NewType _ name _ _ _ ds) = let lds = length ds 
                                                          nds = map derivingName ds
                                                      in mempty {
                                                            sizeStruct = 1
                                                          , gadtStruct = 1
                                                          , newtypeStructContains = 1
                                                          , sizePos = (fromBool $ not $ null ds)
                                                          , gadtPos = (fromBool $ not $ null ds)
                                                          , newtypePosContains = (fromBool $ not $ null ds)
                                                          , sizeDer = lds
                                                          , gadtDer = lds
                                                          , newtypeDerContains = lds
                                                          }
-- ok

-- FAMSTRUCT
------------

-- data
analyzeDecl _ (DataInsDecl _ DataType _  _ ds) = let lds = length ds 
                                                     nds = map derivingName ds
                                                 in mempty {
                                                            sizeStruct = 1
                                                          , famStruct = 1
                                                          , newtypeStructContains = 1
                                                          , sizePos = (fromBool $ not $ null ds)
                                                          , famPos = (fromBool $ not $ null ds)
                                                          , newtypePosContains = (fromBool $ not $ null ds)
                                                          , sizeDer = lds
                                                          , famDer = lds
                                                          , newtypeDerContains = lds
                                                          }

-- ok

-- newtype
analyzeDecl _ (DataInsDecl _ NewType _  _ ds) = let lds = length ds 
                                                    nds = map derivingName ds
                                                in mempty {
                                                            sizeStruct = 1
                                                          , famStruct = 1
                                                          , newtypeStructContains = 1
                                                          , sizePos = (fromBool $ not $ null ds)
                                                          , famPos = (fromBool $ not $ null ds)
                                                          , newtypePosContains = (fromBool $ not $ null ds)
                                                          , sizeDer = lds
                                                          , famDer = lds
                                                          , newtypeDerContains = lds
                                                          }

-- ok

-- GADTFAMSTRUCT
----------------

-- data
analyzeDecl _ (GDataInsDecl _ DataType typeParams _ _ ds) = let lds = length ds 
                                                                nds = map derivingName ds
                                                            in mempty {
                                                            sizeStruct = 1
                                                          , gadtFamStruct = 1
                                                          , newtypeStructContains = 1
                                                          , sizePos = (fromBool $ not $ null ds)
                                                          , gadtFamPos = (fromBool $ not $ null ds)
                                                          , newtypePosContains = (fromBool $ not $ null ds)
                                                          , sizeDer = lds
                                                          , gadtFamDer = lds
                                                          , newtypeDerContains = lds
                                                          }

-- ok

-- newtype
analyzeDecl _ (GDataInsDecl _ NewType typeParams _ _ ds) = let lds = length ds 
                                                               nds = map derivingName ds
                                                           in mempty {
                                                            sizeStruct = 1
                                                          , gadtFamStruct = 1
                                                          , newtypeStructContains = 1
                                                          , sizePos = (fromBool $ not $ null ds)
                                                          , gadtFamPos = (fromBool $ not $ null ds)
                                                          , newtypePosContains = (fromBool $ not $ null ds)
                                                          , sizeDer = lds
                                                          , gadtFamDer = lds
                                                          , newtypeDerContains = lds
                                                          }

-- ok

-- standalone deriving
analyzeDecl _ (DerivDecl _ _ derClass typeParams) = mempty { stdAloneDecl = 1}

analyzeDecl hs (InstDecl _ _ derClass typeParams _) = let isDeriveableInstance = qNameToString derClass `elem` deriveableClasses
                                                      in mempty {
                                                             sizeInst = 1
                                                           , overloadInst = fromBool isDeriveableInstance
                                                           , topOverload = if isDeriveableInstance 
                                                                           then M.singleton (qNameToString derClass) 1 
                                                                           else M.empty
                                                           , dtgOverloadModules = if (qNameToString derClass) == "Data" || (qNameToString derClass) == "Typeable" || (qNameToString derClass) == "Generic"
                                                                                     then [hs]
                                                                                     else []
                                                           }
analyzeDecl _ _ = mempty

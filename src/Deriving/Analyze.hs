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

-- NORMAL
---------

-- data
analyzeDecl (DataDecl _ DataType _ _ name _ ds) = let lds = length ds 
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
analyzeDecl (DataDecl _ NewType _ _ name _ ds) = let lds = length ds 
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
analyzeDecl (GDataDecl _ DataType _ name _ _ _ ds) = let lds = length ds 
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
analyzeDecl (GDataDecl _ NewType _ name _ _ _ ds) = let lds = length ds 
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
analyzeDecl (DataInsDecl _ DataType _  _ ds) = let lds = length ds 
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
analyzeDecl (DataInsDecl _ NewType _  _ ds) = let lds = length ds 
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
analyzeDecl (GDataInsDecl _ DataType typeParams _ _ ds) = let lds = length ds 
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
analyzeDecl (GDataInsDecl _ NewType typeParams _ _ ds) = let lds = length ds 
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
analyzeDecl (DerivDecl _ _ derClass typeParams) = mempty { stdAloneDecl = 1}

analyzeDecl (InstDecl _ _ derClass typeParams _) = let isDeriveableInstance = qNameToString derClass `elem` deriveableClasses
                                                   in mempty {
                                                            sizeInst = 1
                                                          , overloadInst = fromBool isDeriveableInstance
                                                          , topOverload = if isDeriveableInstance 
                                                                          then M.singleton (qNameToString derClass) 1 
                                                                          else M.empty
                                                          }
                                                                      
analyzeDecl _ = mempty

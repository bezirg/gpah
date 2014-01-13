module Generics.GPAH.Deriving.Base where

import Language.Haskell.Exts
import Generics.GPAH.Utils
import Control.DeepSeq
import qualified Data.Map as M
import Data.Monoid

data Analysis = Analysis {
    sizeStruct :: Int
    , normalStruct :: Int
    , gadtStruct :: Int
    , famStruct :: Int
    , gadtFamStruct :: Int
    , newtypeStructContains :: Int
    , sizePos :: Int
    , normalPos :: Int
    , gadtPos :: Int
    , famPos :: Int
    , gadtFamPos :: Int
    , newtypePosContains :: Int
    , sizeDer :: Int
    , normalDer :: Int
    , gadtDer :: Int
    , famDer :: Int
    , gadtFamDer :: Int
    , newtypeDerContains :: Int
    , stdAloneDecl :: Int         -- == stdAloneStruct == stdAlonePos
    , topDer :: M.Map ClassName Int
    , sizeInst :: Int
    , overloadInst :: Int
    , topOverload :: M.Map ClassName Int
    , dtgOverloadModules :: [FilePath]
    }
              deriving (Show)

instance Monoid Analysis where
    mempty = Analysis 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 mempty 0 0 mempty mempty
    (Analysis x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24) `mappend` (Analysis y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22 y23 y24) = Analysis
                                                                                            (x1+y1)
                                                                                            (x2+y2)
                                                                                            (x3+y3)
                                                                                            (x4+y4)
                                                                                            (x5+y5)
                                                                                            (x6+y6)
                                                                                            (x7+y7)
                                                                                            (x8+y8)
                                                                                            (x9+y9)
                                                                                            (x10+y10)
                                                                                            (x11+y11)
                                                                                            (x12+y12)
                                                                                            (x13+y13)
                                                                                            (x14+y14)
                                                                                            (x15+y15)
                                                                                            (x16+y16)
                                                                                            (x17+y17)
                                                                                            (x18+y18)
                                                                                            (x19+y19)
                                                                                            (M.unionWith (+) x20 y20)
                                                                                            (x21+y21)
                                                                                            (x22+y22)
                                                                                            (M.unionWith (+) x23 y23)
                                                                                            (x24 ++ y24)

instance NFData Analysis where
    rnf (Analysis a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24) = a1
                                   `deepseq` a2
                                   `deepseq` a3
                                   `deepseq` a4
                                   `deepseq` a5
                                   `deepseq` a6
                                   `deepseq` a7
                                   `deepseq` a8
                                   `deepseq` a9
                                   `deepseq` a10
                                   `deepseq` a11
                                   `deepseq` a12
                                   `deepseq` a13
                                   `deepseq` a14
                                   `deepseq` a15
                                   `deepseq` a16
                                   `deepseq` a17
                                   `deepseq` a18
                                   `deepseq` a19
                                   `deepseq` a20
                                   `deepseq` a21
                                   `deepseq` a22
                                   `deepseq` a23
                                   `deepseq` a24
                                   `deepseq` ()


-- instead of Normal, more precise DataDecl, GDataDecl, GDataInsDecl
data DerivingType = Normal | StandAlone | Overload
                  deriving (Eq, Read, Show)

instance NFData DerivingType where
   rnf a = a `seq` ()

-- haskell98 [Eq, Ord, Enum, Ix Bounded, Show, Read]
-- ghc   [Data, Typeable, Generic, Functor, Foldable, Traversable]

deriveableClasses = ["Bounded", "Data", "Enum", "Eq", "Functor", "Foldable", "Ix", "Ord", "Read", "Show", "Traversable", "Typeable", "Generic"]
nonGNDClasses = ["Read", "Show", "Typeable","Data"] -- classes that are not **inherited** when in generalized newtype deriving, but instead derived

derivingName :: Deriving -> String
derivingName (qName, _) = qNameToString qName

nameToString :: Name -> String
nameToString (Ident s) = s
nameToString (Symbol s) = s

qNameToString :: QName -> String
qNameToString (UnQual name) = nameToString name
qNameToString (Qual (ModuleName _mname) name) = nameToString name -- we don't have to care about qualified probably
qNameToString (Special specialCon) = show specialCon


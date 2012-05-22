module Deriving.Base where

import Language.Haskell.Exts
import Utils
import Control.DeepSeq
import qualified Data.Map as M
import Data.Monoid

data Analysis = Analysis {
      sizeDer :: Int
    , topDer :: M.Map ClassName Int
    , normalDer :: Int
    , stdAloneDer :: Int
    , newtypeDer :: Int
    , overloadInst :: Int
    }
              deriving (Show)

instance Monoid Analysis where
    mempty = Analysis 0 M.empty 0 0 0 0
    (Analysis x1 x2 x3 x4 x5 x6) `mappend` (Analysis y1 y2 y3 y4 y5 y6) = Analysis
                                                                                            (x1+y1)
                                                                                            (M.unionWith (+) x2 y2)
                                                                                            (x3+y3)
                                                                                            (x4+y4)
                                                                                            (x5+y5)
                                                                                            (x6+y6)


instance NFData Analysis where
    rnf (Analysis a1 a2 a3 a4 a5 a6) = a1
                                   `deepseq` a2
                                   `deepseq` a3
                                   `deepseq` a4
                                   `deepseq` a5
                                   `deepseq` a6
                                   `deepseq` ()


-- type Analysis = M.Map ClassName (M.Map ModuleFileName (M.Map DataName (LineNumber, DerivingType)))

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


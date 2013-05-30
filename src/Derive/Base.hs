module Derive.Base where

import qualified Data.Map as M
import Control.DeepSeq
import Data.Monoid

data Analysis = Analysis {
      deriveTH :: M.Map String Int-- derive directive strings
    , derivePP :: M.Map String Int
    , driftPP :: M.Map String Int
    , driftGL :: M.Map String Int
    }
    deriving (Show)

instance Monoid Analysis where
    mempty = Analysis mempty mempty mempty mempty
    (Analysis x1 x2 x3 x4) `mappend` (Analysis y1 y2 y3 y4) = Analysis
                                                              (M.unionWith (+) x1 y1)
                                                              (M.unionWith (+) x2 y2)
                                                              (M.unionWith (+) x3 y3)
                                                              (M.unionWith (+) x4 y4)
instance NFData Analysis where
    rnf (Analysis a1 a2 a3 a4) = a1 
                                 `deepseq` a2
                                 `deepseq` a3
                                 `deepseq` a4
                                 `deepseq` ()



deriveClasses = ["Arbitrary",
                 "ArbitraryOld",
                 "Arities",
                 "Binary",
                 "BinaryDefer",
                 "Bounded",
                 "Data",
                 "DataAbstract",
                 "Default",
                 "Enum",
                 "EnumCyclic",
                 "Eq",
                 "Fold",
                 "Foldable",
                 "From",
                 "Fucntor",
                 "Has",
                 "Is",
                 "JSON",
                 "LazySet",
                 "Lens",
                 "Monoid",
                 "NFData",
                 "Ord",
                 "Read",
                 "Ref",
                 "Serial",
                 "Serialize",
                 "Set",
                 "Show",
                 "Traversable",
                 "Typeable",
                 "UniplateDirect",
                 "UniplateTypeable",
                 "Update"]
                

driftClasses = ["Binary",
                "GhcBinary",
                "Observable",
                "NFData",
                "Typeable",
                "FuncotrM",
                "HFoldable",
                "Monoid",
                "RMapM",
                "Term",
                "Bounded",
                "Enum",
                "Eq",
                "Ord",
                "Read",
                "Show",
                "ATermConvertible",
                "Haskell2Xml",
                "XmlContent",
                "Parse",
                "Query",
                "from",
                "get",
                "has",
                "is",
                "test",
                "un",
                "update"]


defaultDerivations = ["makeArbitrary",
               "makeArbitraryOld",
               "makeArities",
               "makeBinary",
               "makeBinaryDefer",
               "makeBounded",
               "makeData",
               "makeDataAbstract",
               "makeDefault",
               "makeEnum",
               "makeEnumCyclic",
               "makeEq",
               "makeFold",
               "makeFoldable",
               "makeFrom",
               "makeFunctor",
               "makeHas",
               "makeIs",
               "makeJSON",
               "makeLazySet",
               "makeMonoid",
               "makeNFData",
               "makeOrd",
               "makeRead",
               "makeRef",
               "makeSerial",
               "makeSerialize",
               "makeSet",
               "makeShow",
               "makeTraversable",
               "makeTypeable",
               "makeUniplateDirect",
               "makeUniplateTypeable",
               "makeUpdate"]

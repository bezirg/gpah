module Analysis.Deriving.Base where

import Language.Haskell.Exts
import Analysis.Utils
import Control.DeepSeq
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data
import qualified Data.Map as M



type Analysis = M.Map ClassName (M.Map ModuleFileName (M.Map DataName (LineNumber, DerivingType)))

-- instead of Normal, more precise DataDecl, GDataDecl, GDataInsDecl
data DerivingType = Normal | StandAlone | Overload
                  deriving (Eq, Read, Show)

instance NFData DerivingType where
   rnf a = a `seq` ()

favInstances = ["Bounded", "Data", "Enum", "Eq", "Foldable", "Ix", "Ord", "Read", "Show", "Traversable", "Typeable", "Generic"]



getLeftTyConName :: Type -> DataName
getLeftTyConName x = let res = [qNameToString y | TyCon y <- universe x] in
  if (null res)
  then "error: too complex type"       -- the type that is going to be instantiated is too complex
  else head res

derivingName :: Deriving -> String
derivingName (qName, _) = qNameToString qName

nameToString :: Name -> String
nameToString (Ident s) = s
nameToString (Symbol s) = s

qNameToString :: QName -> String
qNameToString (UnQual name) = nameToString name
qNameToString (Qual (ModuleName _mname) name) = nameToString name -- we don't have to care about qualified probably
qNameToString (Special specialCon) = show specialCon


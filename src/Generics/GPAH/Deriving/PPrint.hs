module Generics.GPAH.Deriving.PPrint where

import Generics.GPAH.Deriving.Base
import Text.CSV
import System.IO
import qualified Data.Map as M
import Data.Function
import Data.List

pprint :: Analysis -> FilePath -> IO ()
pprint (Analysis a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24) fp = do
  let p = [["NumberOfDataStructures (Normal+GADT+FamStruct+GADTFamStruct)", show a1],
           ["NormalStructures", show a2],
           ["GadtStructures", show a3],
           ["FamStructures", show a4],
           ["GadtFamStructures", show a5],
           ["StructuresThatAreNewtype", show a6],
           ["StructuresThatContainAnInlineDeriving", show a7],
           ["NormalThatContainAnInlineDeriving", show a8],
           ["GadtThatContainAnInlineDeriving", show a9],
           ["FamStructThatContainAnInlineDeriving", show a10],
           ["GadtFamStructThatContainAnInlineDeriving", show a11],
           ["NewtypeThatContainAnInlineDeriving", show a12],
           ["NumberOfDerivedInstancesInAnyStruct", show a13],
           ["DerivedInstancesInNormal", show a14],
           ["DerivedInstancesInGadt", show a15],
           ["DerivedInstancesInFamStruct", show a16],
           ["DerivedInstancesInGadtFamStruct", show a17],
           ["DerivedInstancesInNewtype", show a18],
           ["StandaloneDerivingDeclarations", show a19],
           ["TopDerivedClasses", show $ sortBy (flip compare `on` snd) $ M.toList a20],
           ["InstancesChecked", show a21],
           ["InstancesPreferredToWriteManualThanAutoDerive", show $ a22],
           ["TopManual", show $ sortBy (flip compare `on` snd) $ M.toList a23],
           ["DataTypeableGenericManualModules", show $ nub a24],
           ["PossibleGenericity (Data+Typeable+Generic) Derived", show $ sum $ map (flip (M.findWithDefault 0) a20) ["Data","Typeable","Generic"]],
           ["PossibleGenericity (Data+Typeable+Generic) Manual", show $ sum $ map (flip (M.findWithDefault 0) a23) ["Data","Typeable","Generic"]]
          ]
      pCSV = printCSV p

  writeFile fp pCSV

  putStrLn "Deriving Results:\n###############"
  putStrLn pCSV

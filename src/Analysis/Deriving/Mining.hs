module Main where

import Analysis.Deriving.Base
import Analysis.Utils
import qualified Data.Map as M
import Data.List

-- | gives the totall number of occurences of instances
sizeAnalysis :: Analysis -> Int
sizeAnalysis a = M.fold 
                 (\ m acc -> M.fold ((+) . M.size) acc m) 
                 0 a
                 
-- | gives the derived classes in order of most nr of occurences to least nr of occurences 
topAnalysis :: Analysis -> [(ClassName, Int)]
topAnalysis a = sortBy (\ (i1,i2) (j1,j2) -> (i2,i1) `compare` (j2,j1)) $ 
                M.toList $ M.map (M.fold ((+) . M.size) 0) a

-- | gives the number of occurences of a deriving of type d in an analysis a
countDerType :: DerivingType -> Analysis -> Int
countDerType d a = M.fold
                   (\ m acc -> M.fold ((+) . 
                                      M.size . 
                                      M.filter ((== d) 
                                                . snd)) 
                              acc m)
                   0 a

-- | counts the number of classes c in analysis a
countClass :: ClassName -> Analysis -> Int
countClass c a = (M.fold ((+) . M.size) 0) (a M.! c)

countData = countClass "Data"
countTypeable = countClass "Typeable"
countGeneric = countClass "Generic"

countNormal = countDerType Normal
countStandAlone = countDerType StandAlone
countOverload = countDerType Overload

-- Possibly because of newtype-deriving
countNonRegularClasses :: Analysis -> Int
countNonRegularClasses a = M.fold 
                           (\ m acc -> M.fold ((+) . M.size) acc m) 
                           0 (M.filterWithKey (\ k v -> not $ k `elem` favInstances) a)

-- Possible genericity by counting the Data,Typeable and Generic instances
countGenericity :: Analysis -> Int
countGenericity a = M.fold 
                    (\ m acc -> M.fold ((+) . M.size) acc m) 
                    0 (M.filterWithKey (\ k v -> k `elem` ["Data", "Typeable", "Generic"]) a)
                    
                    
                    
main = do
  contents <- getContents
  let resD = read contents :: Analysis

  putStrLn $ "Size of the analysis: " ++ show (sizeAnalysis resD)
  putStrLn $ "Famous list of classes: " ++ show (topAnalysis resD)
  putStrLn $ "Normal Deriving: " ++ show (countNormal resD)
  putStrLn $ "StandAlone Deriving: " ++ show (countStandAlone resD)
  putStrLn $ "Overload Deriving: " ++ show (countOverload resD)
  putStrLn $ "Non-regular classes: " ++ show (countNonRegularClasses resD)
  putStrLn $ "Data instances: " ++ show (countData resD)
  putStrLn $ "Typeable instances: " ++ show (countTypeable resD)
  putStrLn $ "Generic instances: " ++ show (countGeneric resD)
  putStrLn $ "Possible genericity (Data+Typeable+Generic): " ++ show (countGenericity resD)

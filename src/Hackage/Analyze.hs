module Hackage.Analyze where

import Hackage.Base
import Utils
import qualified Data.Map as M

import Data.List (nub, intersect)
import Data.Monoid


import Language.Haskell.Exts
import Language.Haskell.Exts.Comments
import Distribution.PackageDescription
import Distribution.Package

analyzeModule :: ParseResult (Module, [Comment]) -> Analysis
analyzeModule (ParseOk _) = mempty {parsedModules=1}
analyzeModule (ParseFailed (SrcLoc {srcFilename = hs}) _) = mempty {failedModules = [hs]}

analyzePackage :: String -> GenericPackageDescription -> Analysis
analyzePackage pkgname gpkgdesc = Analysis 
                          1 
                          0
                          []
                          (fromBool usesGP) 
                          (fromBool isExe)
                          reverseDeps
    where
      ds = cabalDepends gpkgdesc
      usesGP = any (`elem` ds) libsGP
      isExe = cabalHasExecs gpkgdesc && usesGP
      reverseDeps = M.fromList (zip 
                                     (ds `intersect` libsGP) -- the gp deps of the pkg
                                     (repeat [pkgname])
                               )




cabalHasExecs :: GenericPackageDescription -> Bool
cabalHasExecs (GenericPackageDescription _ _ _ execs _ _) = not $ null execs

cabalDepends :: GenericPackageDescription -> [String]
cabalDepends gpkgdesc = 
  let pds = pkgDepends gpkgdesc
      dName :: Dependency -> String
      dName (Dependency (PackageName s) _) = s
      dNames = nub $ map dName pds
  in dNames


-- Takes a parsed Cabal GenericPackageDescription and return a list of Dependencies
pkgDepends :: GenericPackageDescription -> [Dependency]  
pkgDepends (GenericPackageDescription pkgdesc _ lib execs tests _) = pkgdescDepends pkgdesc ++ libDepends lib ++ execsDepends execs ++ testsDepends tests
  where    
    pkgdescDepends = buildDepends
    libDepends mnode = maybe [] nodeDepends mnode
    execsDepends es  =  concatMap (nodeDepends . snd) es
    testsDepends = execsDepends
    nodeDepends :: CondTree v [Dependency] a -> [Dependency]
    nodeDepends (CondNode {condTreeConstraints = ds}) = ds

module Main where

import Analysis.Utils
import Data.List (nub)
import System.FilePath ((</>))
import Control.Monad (filterM, zipWithM)
import Text.Printf (printf)

-- for Cabal
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity (silent)
import Distribution.Package



cabalHasExecs :: FilePath -> IO Bool
cabalHasExecs fp = do
  (GenericPackageDescription _ _ _ execs _)   <- readPackageDescription silent fp
  return $ not $ null execs


-- Takes a Cabal File and returns its Dependencies' Names
cabalDepends :: FilePath -> IO [String]
cabalDepends fp = do
  gpkgdesc <- readPackageDescription silent fp
  let pds = pkgDepends gpkgdesc
  let dNames = nub $ map dName pds
  return dNames
  where
    dName :: Dependency -> String
    dName (Dependency (PackageName s) _) = s


-- Takes a parsed Cabal GenericPackageDescription and return a list of Dependencies
pkgDepends :: GenericPackageDescription -> [Dependency]  
pkgDepends (GenericPackageDescription pkgdesc _ lib execs tests) = pkgdescDepends pkgdesc ++ libDepends lib ++ execsDepends execs ++ testsDepends tests
  where    
    pkgdescDepends = buildDepends
    libDepends mnode = maybe [] nodeDepends mnode
    execsDepends es  =  concatMap (nodeDepends . snd) es
    testsDepends = execsDepends
    nodeDepends :: CondTree v [Dependency] a -> [Dependency]
    nodeDepends (CondNode {condTreeConstraints = ds}) = ds




main = do

  pkgs <- getSubDirs hackageDir
  vsns <- return . concat =<< mapM getSubDirs (map (hackageDir </>) pkgs)

  let pkgsFullDir = zipWith pkgDir pkgs vsns
  let cabals = zipWith (\ pkg vsn -> hackageDir </> pkg </>  vsn </> pkg ++ "-" ++ vsn </> pkg ++ ".cabal") pkgs vsns -- all packages' cabals
  depends <- mapM cabalDepends cabals -- all packages' dependencies

  -- Count all packages
  let countPkgs = length pkgs

  -- Count modules of all packages
  pkgsSrcDirs <- mapM parseCabalSrcDirs cabals
  let pkgsFullSrcDirs = concat $ zipWith (map . (</>)) pkgsFullDir pkgsSrcDirs
  pkgsHaskellSrcs <- mapM getHaskellSrcs pkgsFullSrcDirs
  let countModules = length $ concat $ pkgsHaskellSrcs

  -- AssocLists: [(Pkg, Depends)]
  let pkgsToDepends = zip pkgs depends
  let pkgsToDependsGP = filter (\ (pkg,ds) -> any (`elem` libsGP) ds) $ pkgsToDepends
  let pkgsToDependsSyb = filter (\ (pkg, ds) -> any (== "syb") ds) $ pkgsToDependsGP
  let pkgsToDependsUni = filter (\ (pkg, ds) -> any (== "uniplate") ds) $ pkgsToDependsGP

  -- Count GP-dependent libraries
  let countLibsGP = length pkgsToDependsGP

  -- Count GP-dependent libraries that have an exe
  let pkgsGP = (map fst pkgsToDependsGP)
  vsnsGP <- return . concat =<< mapM getSubDirs (map (hackageDir </>) pkgsGP)
  let cabalsGP = zipWith (\ pkg vsn -> hackageDir </> pkg </>  vsn </> pkg ++ "-" ++ vsn </> pkg ++ ".cabal") pkgsGP vsnsGP
  execsGP <- filterM cabalHasExecs cabalsGP
  let countExecsGP = length execsGP
  
  -- Count libraries that use SYB and Uniplate
  let countLibsSyb = length pkgsToDependsSyb
  let countLibsUni = length pkgsToDependsUni


  printf "Packages:%d \nModules:%d \nLibsUsingGP:%d \nLibsUsingGPWithExe:%d \nLibsUsingSyb:%d \nLibsUsingUni:%d \n"
    countPkgs countModules countLibsGP countExecsGP countLibsSyb countLibsUni
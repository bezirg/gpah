module Cpp where

import Utils
import Conf
import System.FilePath ((</>))
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent)
import Text.Printf
import Control.Monad
import System.Process
import System.IO
import System.Exit

runCpp = do

  -- package directory structure
  -- e.g. hackage/syb/0.3.6/ACTUAL_PKG_DIR
  let pkgDir pkg vsn = (hackageDirOpt conf) </> pkg </>  vsn </> pkg ++ "-" ++ vsn  

  -- package names-directories
  pkgs <- getSubDirs (hackageDirOpt conf)

  -- package version numbers
  vsns <- return . concat =<< mapM getSubDirs (map ((hackageDirOpt conf) </>) pkgs)

  -- the absolute directories of packages
  let pkgsAbsDirs :: [FilePath]
      pkgsAbsDirs = zipWith pkgDir pkgs vsns

  -- absolute cabal files
  let cabals :: [FilePath]
      cabals =  zipWith (\ pkg vsn -> pkgDir pkg vsn </> pkg ++ ".cabal") pkgs vsns

  -- the parsed cabal descriptions
  parsedCabals <- mapM (readPackageDescription silent) cabals :: IO [GenericPackageDescription]

  -- parse the cabal files and fetch the source directories for each package
  -- relevant to the package
  let pkgsSrcDirs :: [[FilePath]]
      pkgsSrcDirs  = map getSrcDirs parsedCabals

  -- absolute source directories for each package
  let pkgsAbsSrcDirs :: [[FilePath]]
      pkgsAbsSrcDirs = zipWith (map . (</>)) pkgsAbsDirs pkgsSrcDirs

  -- the haskell files for each package
  pkgsHsFiles <- mapM ((liftM concat) . mapM getHaskellSrcs) pkgsAbsSrcDirs :: IO [[FilePath]]

  let dirShell dir cmd = (shell cmd) {cwd = Just dir}

  let buildCompilerCommands :: GenericPackageDescription -> FilePath -> [FilePath] -> [CreateProcess] -- Cabal -> PkgDir -> [PackageHaskellFiles] -> [Commands]
      buildCompilerCommands cabal pkgAbsDir hsFiles = map 
                                            (dirShell pkgAbsDir . printf "ghc -cpp -E -optP-P %s %s -Wwarn %s" (cppOpts cabal) (ghcOpts cabal))
                                            hsFiles

  let compilerCommands = concat $ zipWith3 buildCompilerCommands parsedCabals pkgsAbsDirs pkgsHsFiles

  -- compilerOutHandle <- openFile "cpp.stdout" AppendMode
  -- compilerErrorHandle <- openFile "cpp.stderr" AppendMode

  mapM_ (\ cmd -> createProcess cmd >>= \ (_,_,_,ph) -> waitForProcess ph) compilerCommands


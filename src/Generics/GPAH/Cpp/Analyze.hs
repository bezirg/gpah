module Generics.GPAH.Cpp.Analyze where

import Generics.GPAH.Cpp.Base
import System.Process
import System.Exit
import Text.Printf
import Generics.GPAH.Utils
import Distribution.PackageDescription

analyzeModule :: FilePath -> FilePath -> GenericPackageDescription -> IO Analysis
analyzeModule fp pkgAbsDir cabal = do
  let cmd = (shell 
             (printf "ghc -cpp -E -optP-P %s %s -Wwarn %s" (cppOpts cabal) (ghcOpts cabal) fp)
            ) -- {cwd = Just pkgAbsDir}
  exitCode <- createProcess cmd >>= \ (_,_,_,ph) -> waitForProcess ph
  return $ if exitCode == ExitSuccess then [] else [fp]
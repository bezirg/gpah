module Main where

import Base
import Conf
import Utils

-- the sub-components
import qualified Cpp
import qualified Deriving
import qualified Function
import qualified Date
import qualified Hackage
import qualified Upl
import qualified Interp
import qualified Derive

import Control.Exception (evaluate)
import Control.DeepSeq (force)

import Control.Monad 
import Data.Monoid

    
-- Cabal-related imports
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent)
import Distribution.Package

import System.FilePath

main = analyze >>= pprint

analyze :: IO Analysis
analyze = if hasSubComponent conf -- check for enabled subcomponents, so not to pointlessly traverse the modules
          then (getHackagePkgsNames >>= mapM analyzePkg >>= return . mconcat >>= appendAnalyzeDate)
          else return mempty

appendAnalyzeDate :: Analysis -> IO Analysis
appendAnalyzeDate a = maybe 
                      (return a) -- not enabled in the conf, skip
                      (const $ do
                         contents <- readFile (hackageLogOpt conf)
                         return $ Date.analyzeHackageLog contents a `mappend` a -- appends it to the main analysis
                      )
                      (dateOpt conf)


analyzePkg :: String -> IO Analysis
analyzePkg pkgName = do
  vsn <- getPkgVersion pkgName

  let cabal =  pkgCabal pkgName vsn  :: FilePath -- absolute cabal files
  parsedCabal <- readPackageDescription silent cabal :: IO GenericPackageDescription

  let pkgAbsDir = pkgDir pkgName vsn    -- the absolute directories of package
  let pkgSrcDirs  = getSrcDirs parsedCabal  :: [FilePath]   -- fetch the source directories , relevant to package
  let pkgAbsSrcDirs = map (pkgAbsDir </>) pkgSrcDirs :: [FilePath]  -- absolute source directories
  
  haskellSrcs <- return . concat . zipWith (\ abssrcdir srcs -> zip (repeat abssrcdir) srcs) pkgAbsSrcDirs =<< mapM getHaskellSrcs pkgAbsSrcDirs

  let hacPkg = maybe mempty (const $ Hackage.analyzePackage pkgName parsedCabal) (hackageOpt conf `mplus` dateOpt conf `mplus` intOpt conf `mplus` dveOpt conf)  -- -t or -i or -e, imply -h

  let analyzeModule (pkgAbsSrcDir, hs) = do
                       cpp <- maybe (return mempty) (const $ Cpp.analyzeModule hs pkgAbsDir parsedCabal) (cppOpt conf)
                             
                       int <- maybe (return mempty) (const $ Interp.analyzeModule hs pkgName pkgAbsSrcDir hacPkg) (intOpt conf)

                       dve <- maybe (return mempty) (const $ Derive.analyzeModule hs hacPkg) (dveOpt conf)

                       parsedMdl <- parseModuleFile hs

                       -- turn on specific sub analyses based on user-provided conf
                       let der = maybe mempty (const $ Deriving.analyzeModule parsedMdl) (derivingOpt conf)
                           fun = maybe mempty (const $ Function.analyzeModule parsedMdl parsedCabal) (functionOpt conf)
                           upl = maybe mempty (const $ Upl.analyzeModule parsedMdl parsedCabal) (uniplateOpt conf)
                           hac = maybe mempty (const $ Hackage.analyzeModule parsedMdl) (hackageOpt conf)
                                        
                       evaluate . force $ Analysis cpp der fun upl hac mempty int dve


  let appendAnalyzeHacPkg a = a { hacAnalysis = hacAnalysis a `mappend` hacPkg }

  return . appendAnalyzeHacPkg . mconcat =<< mapM analyzeModule haskellSrcs



pprint :: Analysis -> IO ()
pprint (Analysis cpp der fun upl hac dte int dve) = do
  maybe (return ()) (Cpp.pprint cpp) (cppOpt conf)
  maybe (return ()) (Hackage.pprint hac) (hackageOpt conf)
  maybe (return ()) (Deriving.pprint der) (derivingOpt conf)
  maybe (return ()) (Function.pprint fun) (functionOpt conf)
  maybe (return ()) (Upl.pprint upl) (uniplateOpt conf)
  maybe (return ()) (Date.pprint dte) (dateOpt conf)
  maybe (return ()) (Interp.pprint int) (intOpt conf)
  maybe (return ()) (Derive.pprint dve) (dveOpt conf)

module Analysis.Utils (module Analysis.Conf,
                       pkgDir,
                       getSubDirs,
                       getSrcDirs,
                       usesGP,
                       parseCabalSrcDirs,
                       getHaskellSrcs,
                       libsGP,
                       libsUsingSyb,
                       libsUsingUni,
                       ClassName,
                       DataName,
                       LineNumber,
                       ModuleFileName,
                       removePragmas
                      ) where

import Analysis.Conf

import System.FilePath ((</>))
import System.Directory
import Data.List
import Control.Monad (filterM)


-- Cabal-related imports
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent)
import Distribution.Package


pkgDir pkg vsn = hackageDir </> pkg </>  vsn </> pkg ++ "-" ++ vsn  

getSubDirs fp = do 
  ls <- getDirectoryContents fp 
  let clean_ls = ls \\ [".",".."]
  subdirs <- filterM (doesDirectoryExist . (fp </>)) clean_ls
  return subdirs


getSrcDirs :: GenericPackageDescription -> [FilePath]
getSrcDirs (GenericPackageDescription {condLibrary = l, condExecutables = es, condTestSuites = ts}) = 
  let getSrcDirsLib = hsSourceDirs . libBuildInfo 
      getSrcDirsExec  = hsSourceDirs . buildInfo
      getSrcDirsTest = hsSourceDirs . testBuildInfo
      pkgSrcDirs = nub $ concat $ 
                   maybe [[]] ((: []) . getSrcDirsLib . condTreeData) l ++ 
                   map (getSrcDirsExec . condTreeData . snd) es ++ 
                   map (getSrcDirsTest . condTreeData . snd) ts
  in if null pkgSrcDirs 
     then [""] -- the srcdir of the package is its home dir
     else pkgSrcDirs

usesGP :: String -> Bool
usesGP pkg = pkg `elem` (libsUsingSyb ++ libsUsingUni)

parseCabalSrcDirs :: FilePath -> IO [String]
parseCabalSrcDirs fp = do
  gpkgdesc <- readPackageDescription silent fp
  return (getSrcDirs gpkgdesc)


getHaskellSrcs :: FilePath -> IO [FilePath]
getHaskellSrcs fp = do
  existsDir <- doesDirectoryExist fp -- it's possible that the srcdir is not there
  if existsDir 
    then do  
    ls <- getDirectoryContents fp 
    -- filter hs, lhs and not Setup.hs and Setup.lhs and no hidden
    let hs_lhs = filter (\ l -> (".hs" `isSuffixOf` l || ".lhs" `isSuffixOf` l) && not ("." `isPrefixOf` l) && (l /= "Setup.hs") && (l /= "Setup.lhs")) ls
    subDirs <- getSubDirs fp

    if null subDirs 
      then do
      return $ map (fp </>) hs_lhs
      else do
      hs_lhs' <- mapM getHaskellSrcs (map (fp </>) subDirs)
      return $ map (fp </>) hs_lhs ++ (concat hs_lhs')
    else do 
    return []


type ClassName = String
type DataName = String
type LineNumber = Int
type ModuleFileName = String

removePragmas :: String -> String
removePragmas s = unlines $ map removePragma (lines s) where
  removePragma ('#':xs) = ""
  removePragma xs = xs


-- Libraries under the "Generics" category of HackageDB
libsGP = ["alloy",
              "alloy-proxy-fd",
              "Annotations",
              "compdata",
              "derive-IG",
              "emgm",
              "functorm",
              "gdiff-ig",
              "generic-deriving",
              "GenericPretty",
              "guarded-rewriting",
              "instant-generics",
              "instant-zipper",
              "ligd",
              "ListLike",
              "listlike-instances",
              "multiplate",
              "multiplate-simplified",
              "multirec",
              "multirec-alt-deriver",
              "multirec-binary",
              "pointless-haskell",
              "pointless-lenses",
              "pointless-rewrite",
              "regular",
              "regular-extras",
              "regular-web",
              "RepLib",
              "rewriting",
              "special-functors",
              "spine",
              "StrategyLib",
              "syb",
              "syz",
              "unbound",
              "uniplate",
              "xformat",
              "zipper"]
         
libsUsingSyb = ["HaRe","lambdabot","binary-generic","Hermes","WebBits","AGI","CSPM-Interpreter","CSPM-Frontend","Hs2lib","type-settheory","property-list","CSPM-cspm","pugs-compat","haskell-src-exts-qq","happstack-hsp","svg2q","cmdlib","JsContracts","DataTreeView","WebBits-Html","activehs","hssqlppp","typehash","haskell-src-meta","haskell-platform-test","emgm","prolog","fresh","symbol","haskell-src-meta-mwotton","ghc-syb-utils","ixdopp","KiCS","GeoIp","instant-generics","text-xml-generic","hastache","Interpolation","preprocessor-tools","genprog","hmumps","DSH","th-expand-syns","nikepub","SVG2Q","graph-utils","hobbits","astview-utils","lambdabot-utils","KiCS-debugger","jmacro","DPM","multirec-alt-deriver","todos","LslPlus","Agda","fibon","srcloc","astview","zeroth","logic-classes","alms","set-extra","aeson-native","language-c-quote","ChasingBottoms","ginsu","atom","aeson","logic-TPTP","stdata","type-level","derive","DrHylo","cflp","url-generic","random-fu","lhs2TeX-hl","dewdrop"]

libsUsingUni = ["fix-imports","ycextra","hylolib","ideas","graphtype","syntax-trees","supero","hssqlppp","yi","simpleprelude","hlint","GenI","visual-prof","scyther-proof","ottparse-pretty","scion","hoogle","cabal-query","compdata","hugs2yc","hgettext","yhccore","tempus","derive","optimusprime"]




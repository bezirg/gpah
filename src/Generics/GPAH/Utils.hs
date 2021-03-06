module Generics.GPAH.Utils  where
    
import Generics.GPAH.Conf

import System.FilePath
import System.Directory
import Data.List
import Control.Monad (filterM, liftM)
import Data.Char (isSpace)
import Language.Haskell.Exts
import Language.Haskell.Exts.Comments
import Network.HTTP
import Network.URI
import Data.Maybe (fromJust)

import System.IO
import qualified Data.ByteString.Char8 as B

-- Cabal-related imports
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent)
import Distribution.Package
import qualified Distribution.Compiler as Compiler (CompilerFlavor (GHC))

type ClassName = String
type DataName = String
type LineNumber = Int
type ModuleFileName = String

fromBool :: Bool -> Int
fromBool True = 1
fromBool False = 0

toBool :: Int -> Bool
toBool 1 = True
toBool 0 = False

-- returns the package directory relative to the hackage archive
-- e.g. hackage/syb/0.3.6/ACTUAL_PKG_DIR
pkgDir  :: String -> String -> FilePath
pkgDir pkg vsn = hackageDirOpt conf </> pkg </>  vsn </> pkg ++ "-" ++ vsn

pkgCabal :: FilePath -> FilePath -> FilePath
pkgCabal pkg vsn = pkgDir pkg vsn </> pkg ++ ".cabal"

-- ignore package hsc3, because of error:
-- gpanalysis.exe: In file hackage\hsc3\0.14\hsc3-0.14\Help\UGen\Panner\splay.help.lhs at line 1: program line before comment line.
-- 
getHackagePkgsNames :: IO [String]
getHackagePkgsNames = liftM (delete "hsc3") $ getSubDirs (hackageDirOpt conf)


getPkgVersion :: String -> IO String
getPkgVersion pkgName = liftM head $ getSubDirs (hackageDirOpt conf </> pkgName)

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

-- if using Data.Text, this function is provided
-- and is more efficient
strip      :: String -> String
strip      = f . f
    where f = reverse . dropWhile isSpace


ghcOpts :: GenericPackageDescription -> String
ghcOpts (GenericPackageDescription {condLibrary = l, condExecutables = es, condTestSuites = ts}) =
    unwords [libGhcOptions,
             exesGhcOptions,
             testsGhcOptions]
    where
      libGhcOptions = maybe "" (ghcOptionsBI . libBuildInfo . condTreeData)  l
      exesGhcOptions = unwords $ map (ghcOptionsBI . buildInfo .  condTreeData . snd) es
      testsGhcOptions = unwords $ map (ghcOptionsBI . testBuildInfo .  condTreeData . snd) ts
      ghcOptionsBI :: BuildInfo -> String
      ghcOptionsBI (BuildInfo {options = opts}) = maybe "" unwords $ lookup Compiler.GHC opts


      
cppOpts :: GenericPackageDescription -> String
cppOpts (GenericPackageDescription {condLibrary = l, condExecutables = es, condTestSuites = ts}) =
    (strip . unwords) [libCppOptions,
                       exesCppOptions,
                       testsCppOptions,
                       libIncludeDirs,
                       exesIncludeDirs,
                       testsIncludeDirs]
    where
      libCppOptions = maybe "" (cppOptionsBI . libBuildInfo . condTreeData)  l
      exesCppOptions = unwords $ map (cppOptionsBI . buildInfo .  condTreeData . snd) es
      testsCppOptions = unwords $ map (cppOptionsBI . testBuildInfo .  condTreeData . snd) ts
      cppOptionsBI :: BuildInfo -> String
      cppOptionsBI (BuildInfo {cppOptions = opts}) = unwords opts

      libIncludeDirs = maybe "" (includeDirsBI . libBuildInfo . condTreeData)  l
      exesIncludeDirs = unwords $ map (includeDirsBI . buildInfo .  condTreeData . snd) es
      testsIncludeDirs = unwords $ map (includeDirsBI . testBuildInfo .  condTreeData . snd) ts
      includeDirsBI :: BuildInfo -> String
      includeDirsBI (BuildInfo {includeDirs = opts}) = unwords $ map ("-I" ++) opts
      


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

    -- It prefers the ".hspp" version of the source file, over the ".hs" or ".lhs" version of it
    hs_lhs_hspp <- mapM (\ hsOrlhsFile -> do 
                          let hsppFile = addExtension (dropExtension hsOrlhsFile) ".hspp"
                          p <- doesFileExist (fp </> hsppFile)
                          return $ if p then hsppFile else hsOrlhsFile)
                       hs_lhs

    subDirs <- getSubDirs fp

    if null subDirs 
      then do
      return $ map (fp </>) hs_lhs_hspp
      else do
      hs_lhs_hspp' <- mapM getHaskellSrcs (map (fp </>) subDirs)
      return $ map (fp </>) hs_lhs_hspp ++ (concat hs_lhs_hspp')
    else do 
    return []


parseModuleFile :: FilePath -> IO (ParseResult (Module, [Comment]))
parseModuleFile fp = do

  h <- openFile fp ReadMode

  te <- mkTextEncoding "UTF-8//IGNORE"
  hSetEncoding h te

  contents <-  return . B.unpack =<< B.hGetContents h

  hClose h

  -- remove pragmas
  let clearedContents = removePragmas contents

  -- set parsing mode
  let parseMode = defaultParseMode { parseFilename = fp
                                   , extensions = knownExtensions -- support extensions
                                   , fixities = Nothing }         -- no infix operators
  return $ parseFileContentsWithComments parseMode clearedContents


removePragmas :: String -> String
removePragmas s = unlines $ map removePragma (lines s) where
  removePragma ('#':xs) = ""
  removePragma xs = xs


downloadURL :: String -> IO (Either String String)
downloadURL url =
    do resp <- simpleHTTP request
       case resp of
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r -> 
             case rspCode r of
               (2,_,_) -> return $ Right (rspBody r)
               (3,_,_) -> -- A HTTP redirect
                 case findHeader HdrLocation r of
                   Nothing -> return $ Left (show r)
                   Just url -> downloadURL url
               _ -> return $ Left (show r)
    where request = Request {rqURI = uri,
                             rqMethod = GET,
                             rqHeaders = [],
                             rqBody = ""}
          uri = fromJust $ parseURI url


-- Libraries under the "Generics" , "Generic" categories of HackageDB + "syb-with-class" + "derive" + "DrIFT-cabalized"
libsGP = ["HsTools",
          "syb-extras",
          "algebraic-classes",
          "alloy",
          "alloy-proxy-fd",
          "Annotations",
          "compdata",
          "derive-IG",
          "emgm",
          "fixplate",
          "functorm",
          "gdiff-ig",
          "gdiff-th",
          "generic-binary",
          "generic-deepseq",
          "generic-deriving",
          "GenericPretty",
          "geniplate",
          "guarded-rewriting",
          "instant-generics",
          "instant-zipper",
          "ligd",
          "ListLike",
          "listlike-instances",
          "multifocal",
          "multiplate",
          "multiplate-simplified",
          "multirec",
          "multirec-alt-deriver",
          "multirec-binary",
          "one-liner",
          "pointless-haskell",
          "pointless-lenses",
          "pointless-rewrite",
          "putlenses",
          "regular",
          "regular-extras",
          "regular-web",
          "RepLib",
          "rewriting",
          "special-functors",
          "spine",
          "Strafunski-StrategyLib",
          "StrategyLib",
          "syb",
          "syz",
          "TYB",
          "unbound",
          "unfoldable",
          "uniplate",
          "xformat",
          "yoko",
          "zipper",
          "syb-with-class",
          "derive",
          "DrIFT-cabalized"
         ]



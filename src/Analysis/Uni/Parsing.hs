module Main where

import Analysis.Uni.Base
import Analysis.Utils
import Language.Haskell.Exts
import qualified Data.Map as M
import System.FilePath ((</>))

-- for encoding conversion
import qualified Codec.Text.IConv as IConv
import qualified Data.ByteString.Lazy.Char8 as B

-- for strictness
import Control.Exception (evaluate)
import Control.DeepSeq

analyseImports :: [ImportDecl] -> Maybe UniStyle
analyseImports is = if isMixed is
                    then Just Mixed
                    else analyseImports' is where
                      isMixed is = any (\ (ImportDecl { importModule = ModuleName mn }) -> mn == mixed) is
                      analyseImports' (ImportDecl { importModule = ModuleName mn} :xs) | mn `elem` manualSpeed = Just ManualSpeed
                      analyseImports' (ImportDecl { importModule = ModuleName mn} :xs) | mn `elem` automatic = Just Automatic
                      analyseImports' (ImportDecl { importModule = ModuleName mn} :xs) | mn `elem` manualPointless = Just ManualPointless
                      analyseImports' (_:xs) = analyseImports' xs
                      analyseImports' [] = Nothing


analyseModule :: Module -> Analysis
analyseModule (Module (SrcLoc {srcFilename = fn}) _ _ _ _ imports _) = maybe M.empty (\ s -> M.singleton s [fn]) $  analyseImports imports


parseAnalyseModuleFile :: FilePath -> IO Analysis
parseAnalyseModuleFile fp = do
  contents <-  fmap (B.unpack . IConv.convertFuzzy IConv.Discard "UTF-8" "UTF-8") (B.readFile fp)
  let clearedContents = removePragmas contents
  let parseMode = defaultParseMode { parseFilename = fp, extensions = knownExtensions, fixities = Nothing }
  case parseFileContentsWithMode parseMode clearedContents of
    ParseOk m -> evaluate (analyseModule m)
    _ -> evaluate M.empty -- Parse Failed, Empty Analysis

  
  
main = do
  pkgs <- getSubDirs hackageDir
  vsns <- return . concat =<< mapM getSubDirs (map (hackageDir </>) pkgs)
  let pkgDir pkg vsn = hackageDir </> pkg </>  vsn </> pkg ++ "-" ++ vsn  
  let pkgsFullDir = zipWith pkgDir pkgs vsns
  let cabals =  zipWith (\ pkg vsn -> pkgDir pkg vsn </> pkg ++ ".cabal") pkgs vsns
  pkgsSrcDirs <- mapM parseCabalSrcDirs cabals
  let pkgsFullSrcDirs = concat $ zipWith (map . (</>)) pkgsFullDir pkgsSrcDirs
  haskellSrcs <- mapM getHaskellSrcs pkgsFullSrcDirs


  -- Only analyse Uni-dependent packages
  let pkgsUni = filter (`elem` libsUsingUni) pkgs
  vsnsUni <- return . concat =<< mapM getSubDirs (map (hackageDir </>) pkgsUni)
  let pkgDirUni pkgUni vsnUni = hackageDir </> pkgUni </>  vsnUni </> pkgUni ++ "-" ++ vsnUni  
  let pkgsFullDirUni = zipWith pkgDirUni pkgsUni vsnsUni
  let cabalsUni =  zipWith (\ pkgUni vsnUni -> pkgDirUni pkgUni vsnUni </> pkgUni ++ ".cabal") pkgsUni vsnsUni
  pkgsSrcDirsUni <- mapM parseCabalSrcDirs cabalsUni
  let pkgsFullSrcDirsUni = concat $ zipWith (map . (</>)) pkgsFullDirUni pkgsSrcDirsUni
  haskellSrcsUni <- mapM getHaskellSrcs pkgsFullSrcDirsUni
  resUni <- mapM 
         (\ mf -> parseAnalyseModuleFile mf) 
         (concat haskellSrcsUni) 
  let unionedResUni = M.unionsWith (++) resUni

  putStr (show unionedResUni)

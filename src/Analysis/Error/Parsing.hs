module Main where

import Analysis.Error.Base
import Analysis.Utils
import Language.Haskell.Exts
import Control.Monad (foldM)
import System.FilePath ((</>))
import System.IO

-- for encoding conversion
import qualified Codec.Text.IConv as IConv
import qualified Data.ByteString.Lazy.Char8 as B

-- for strictness
import Control.Exception (evaluate)
import Control.DeepSeq



parseAnalyseModuleFile :: FilePath -> IO Analysis
parseAnalyseModuleFile fp = do
  -- discard characters that fail to convert to utf-8
  contents <-  fmap (B.unpack . IConv.convertFuzzy IConv.Discard "UTF-8" "UTF-8") (B.readFile fp)

  -- remove #ifdef-style pragmas, cannot be parsed by HSE
  let clearedContents = removePragmas contents

  -- the ParseMode is enabled with all Extensions, but no fixities
  let parseMode = defaultParseMode { parseFilename = fp, extensions = knownExtensions, fixities = Nothing }

  case parseFileContentsWithMode parseMode clearedContents of
    ParseFailed (SrcLoc {srcFilename = fn}) _ -> evaluate [fn]
    _ -> evaluate []

main = do
  -- analysis on whole Hackage
  pkgs <- getSubDirs hackageDir
  vsns <- return . concat =<< mapM getSubDirs (map (hackageDir </>) pkgs)
  let pkgDir pkg vsn = hackageDir </> pkg </>  vsn </> pkg ++ "-" ++ vsn  
  let pkgsFullDir = zipWith pkgDir pkgs vsns
  let cabals =  zipWith (\ pkg vsn -> pkgDir pkg vsn </> pkg ++ ".cabal") pkgs vsns
  pkgsSrcDirs <- mapM parseCabalSrcDirs cabals
  let pkgsFullSrcDirs = concat $ zipWith (map . (</>)) pkgsFullDir pkgsSrcDirs
  haskellSrcs <- mapM getHaskellSrcs pkgsFullSrcDirs

  resE <- foldM                  -- fold the Analysis result for each module
         (\ acc mf -> parseAnalyseModuleFile mf >>= evaluate . force .  (++) acc)
         ([] :: Analysis)
         (concat haskellSrcs) 

  putStr (show resE)

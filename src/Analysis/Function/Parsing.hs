module Main where

import Analysis.Function.Base
import Analysis.Utils
import Language.Haskell.Exts
import System.IO
import System.FilePath ((</>))
import Control.Monad (foldM)

-- for encoding conversion
import qualified Codec.Text.IConv as IConv
import qualified Data.ByteString.Lazy.Char8 as B

-- for strictness
import Control.Exception (evaluate)
import Control.DeepSeq


import Data.Generics
import qualified Data.List as L
import qualified Data.Map as M
import Debug.Trace

-- | adds the module names to the analysis results
addModuleFileName' :: String -> M.Map FavFunction Occs -> Analysis
addModuleFileName' mfn rr = M.map (\ row -> M.singleton mfn row) rr

-- | Constructs a list of keywords to search for
constructSearchList :: M.Map String [String] -> [String]
constructSearchList m = foldr (++) [] $ M.elems m

-- | Gives all the importnames of a module
--getImportNames :: Module -> [String]
--getImportNames (Module _ _ _ _ _ is _) = map ((\(ModuleName s) -> s) . importModule) is

-- | if the given variable is the given function to search for a 1 is returned else a 0
findN :: FavFunction -> Exp -> Int
findN n e@(Var n') = case n' of 
                       UnQual i -> case i of
                                     Ident a  -> if a == n then 1 else 0
                                     Symbol a -> if a == n then 1 else 0
                       Qual _ i -> case i of
                                     Ident a  -> if a == n then 1 else 0
                                     Symbol a -> if a == n then 1 else 0
                       _        -> 0
findN _ _          = 0                                 

-- | collects for a given module the number of occurences of a given function
collectOccs :: Module -> FavFunction -> Int
collectOccs m n = everything (+) (0 `mkQ` findN n) m

-- | executes the function analysis on a given module
analyseModule :: Module -> Analysis
analyseModule modul@(Module sloc _ _ _ _ _ _) = 
  let toSearch = L.nub $ constructSearchList {-(getImportNames modul)-} favorites
      results  = addModuleFileName' (srcFilename sloc) $ M.fromList $ filter (\(_,nr) -> nr > 0) $ map (\x -> (x, collectOccs modul x)) toSearch
  in results

-- | parses and analyses a module on the given filepath
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

  -- For Function Analysis
  let pkgsF = filter usesGP pkgs
  vsnsF <- return . concat =<< mapM getSubDirs (map (hackageDir </>) pkgsF)
  let pkgDirF pkgF vsnF = hackageDir </> pkgF </>  vsnF </> pkgF ++ "-" ++ vsnF  
  let pkgsFullDirF = zipWith pkgDirF pkgsF vsnsF
  let cabalsF =  zipWith (\ pkgF vsnF -> pkgDirF pkgF vsnF </> pkgF ++ ".cabal") pkgsF vsnsF
  pkgsSrcDirsF <- mapM parseCabalSrcDirs cabalsF
  let pkgsFullSrcDirsF = concat $ zipWith (map . (</>)) pkgsFullDirF pkgsSrcDirsF
  haskellSrcsF <- mapM getHaskellSrcs pkgsFullSrcDirsF

  resF <- foldM 
         (\ acc mf -> parseAnalyseModuleFile mf >>= evaluate . force .  M.unionWith M.union acc)
         (M.empty :: Analysis)
         (concat haskellSrcsF) 

  putStr (show resF)







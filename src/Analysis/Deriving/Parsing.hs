{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, ScopedTypeVariables #-}
module Main where

import Analysis.Deriving.Base
import Analysis.Utils
import Language.Haskell.Exts
import System.Environment (getArgs)
import qualified Data.Map as M
import Control.Monad (filterM, zipWithM, foldM)
import System.IO
import System.Directory
import Data.List (nub, (\\), isPrefixOf, isSuffixOf)
import System.FilePath ((</>))

-- for encoding conversion
import qualified Codec.Text.IConv as IConv
import qualified Data.ByteString.Lazy.Char8 as B

-- for strictness
import Control.Exception (evaluate)
import Control.DeepSeq

-- Syb and Uniplate
import Generics.SYB
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data


countDerivingClasses :: GenericQ (M.Map ClassName (M.Map DataName (LineNumber, DerivingType)))
countDerivingClasses x = everything (M.unionWith M.union) (M.empty `mkQ` derivingDecl) x


derivingDecl :: Decl -> M.Map ClassName (M.Map DataName (LineNumber, DerivingType))
derivingDecl (DataDecl sloc _ _ name _ _ ds) = M.fromList $ map 
                                               (\ d -> (derivingName d, -- key
                                                       M.singleton (nameToString name) (srcLine sloc, Normal))  -- value
                                               )
                                               ds -- all the derivings in the datatype
derivingDecl (GDataDecl sloc _ _ name _ _ _ ds) = M.fromList $ map 
                                                  (\ d -> (derivingName d,  -- key
                                                          M.singleton (nameToString name) (srcLine sloc, Normal))  -- value
                                                  )
                                                  ds
derivingDecl (GDataInsDecl sloc _ typeParams _ _ ds) = M.fromList $ map 
                                                        (\ d -> (derivingName d, -- key
                                                                M.singleton (getLeftTyConName typeParams) (srcLine sloc, Normal)) -- value
                                                        ) 
                                                        ds
derivingDecl (DerivDecl sloc _ derClass typeParams) = M.singleton 
                                                      (qNameToString derClass) -- key
                                                      (M.singleton (getLeftTyConName (head typeParams)) (srcLine sloc, StandAlone)) -- value

derivingDecl (InstDecl sloc _ derClass typeParams _) = let className = qNameToString derClass  in 
   if className `elem` favInstances
   then M.singleton 
        className -- key
        (M.singleton (getLeftTyConName (head typeParams)) (srcLine sloc, Overload)) -- value
   else M.empty
derivingDecl _ = M.empty

analyseModule :: Module -> Analysis
analyseModule m@(Module sloc _ _ _ _ _ _) = addModuleFileName (srcFilename sloc) (countDerivingClasses m) where
  addModuleFileName :: String -> M.Map ClassName (M.Map DataName (LineNumber, DerivingType)) -> Analysis
  addModuleFileName mfn rr = M.map (\ row -> M.singleton mfn row) rr

parseAnalyseModuleFile :: FilePath -> IO Analysis
parseAnalyseModuleFile fp = do
  contents <-  fmap (B.unpack . IConv.convertFuzzy IConv.Discard "UTF-8" "UTF-8") (B.readFile fp)
  let clearedContents = removePragmas contents
  let parseMode = defaultParseMode { parseFilename = fp, extensions = knownExtensions, fixities = Nothing }
  case parseFileContentsWithMode parseMode clearedContents of
    ParseOk m -> evaluate (analyseModule m)
    _ -> evaluate M.empty -- Parse Failed, Empty Analysis
              
              
  
main = do
  -- For Deriving Analysis
  pkgs <- getSubDirs hackageDir
  vsns <- return . concat =<< mapM getSubDirs (map (hackageDir </>) pkgs)
  let pkgDir pkg vsn = hackageDir </> pkg </>  vsn </> pkg ++ "-" ++ vsn  
  let pkgsFullDir = zipWith pkgDir pkgs vsns
  let cabals =  zipWith (\ pkg vsn -> pkgDir pkg vsn </> pkg ++ ".cabal") pkgs vsns
  pkgsSrcDirs <- mapM parseCabalSrcDirs cabals
  let pkgsFullSrcDirs = concat $ zipWith (map . (</>)) pkgsFullDir pkgsSrcDirs
  haskellSrcs <- mapM getHaskellSrcs pkgsFullSrcDirs

  resD <- foldM 
         (\ acc mf -> parseAnalyseModuleFile mf >>= evaluate . force .  M.unionWith M.union acc)
         (M.empty :: Analysis)
         (concat haskellSrcs) 

  putStr (show resD)

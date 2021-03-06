module Generics.GPAH.Upl.Analyze where

import Generics.GPAH.Upl.Base
import Generics.GPAH.Utils
import Language.Haskell.Exts
import Language.Haskell.Exts.Comments
import qualified Data.Map as M
import Generics.GPAH.Hackage.Analyze

import Data.Monoid

-- Cabal-related imports
import Distribution.PackageDescription
import Distribution.Package

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


analyzeModule :: ParseResult (Module, [Comment]) -> GenericPackageDescription -> Analysis
analyzeModule (ParseOk ((Module (SrcLoc {srcFilename = fn}) _ _ _ _ imports _),_)) gpkgdesc =
    if "uniplate" `elem` cabalDepends gpkgdesc -- if has uniplate as dep
    then Analysis $ maybe M.empty (\ s -> M.singleton s [fn]) $  analyseImports imports
    else mempty
analyzeModule _ _ = mempty


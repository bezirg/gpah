module Generics.GPAH.Interp.Analyze where

import Generics.GPAH.Interp.Base
import qualified Generics.GPAH.Hackage.Base as Hackage

import Generics.GPAH.Utils
import qualified Data.Map as M
import Data.List
import System.FilePath    
import Data.Monoid
import Language.Haskell.Interpreter

analyzeModule :: FilePath -> FilePath -> FilePath -> Hackage.Analysis -> IO Analysis
analyzeModule hs pkgName pkgAbsSrcDir hacPkg | pkgName `elem` excludePkgs = return mempty -- some pkgs are excluded
                                             | otherwise = let moduleName = map (\ c -> if isPathSeparator c then '.' else c) -- make the / to .
                                                                            (dropExtension (makeRelative pkgAbsSrcDir hs))
                                                               interpr :: Interpreter [String]
                                                               interpr = do
                                                                 set [searchPath:=[pkgAbsSrcDir]]
                                                                 -- the interpreter takes .hs not .hspp
                                                                 loadModules [replaceExtension hs (if pkgName `elem` ["HaRe","KiCS-debugger","couchdb-conduit"] then ".lhs" else ".hs")]
                                                                 setTopLevelModules [moduleName]
                                                                 es <- getModuleExports moduleName -- :browse equivalent
                                                                 ts <- sequence [typeOf x | e@(Fun x) <- es] -- typecheck the function of the module
                                                                 let fts = filter (\ t -> any (`isInfixOf` t) intTypes) ts -- the type signatures that contain syb or uniplate
                                                                 return fts
                                                           in if any (`elem` (M.keys (Hackage.reverseDeps hacPkg))) ["syb","uniplate"] -- does the package have syb or uniplate as revdep
                                                              then print hs >> runInterpreter interpr >>= either 
                                                                       (\ res -> print res >> return mempty) -- failed to interpret
                                                                       (\ res -> return $ Analysis $ if null res then M.empty else M.singleton pkgName res)
                                                              else return mempty -- module skipped

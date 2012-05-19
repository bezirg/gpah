{-# LANGUAGE DeriveDataTypeable #-}

module Conf where

import System.IO.Unsafe
import System.Console.CmdArgs
import Control.Monad
import Data.Maybe (maybe)

-- easier to have a top-level state for the conf
conf = unsafePerformIO getConf

getConf = cmdArgs confOpt

data Conf = Conf {
    derivingOpt :: Maybe FilePath
    , functionOpt :: Maybe FilePath
    , uniplateOpt :: Maybe FilePath
    , hackageOpt :: Maybe FilePath
    , dateOpt :: Maybe FilePath
    , hackageDirOpt :: FilePath
    , hackageLogOpt :: FilePath
    , cppOpt :: Bool
  } deriving (Show, Eq, Data, Typeable)

hasSubComponent :: Conf -> Bool
hasSubComponent (Conf der fun upl hac dte _ _ _) = 
    maybe False (const True)
              (der `mplus` fun `mplus` upl `mplus` hac `mplus` dte)

confOpt = Conf {
             derivingOpt = def &= explicit  &= name "d" &= name "deriving" &= opt "./results/d_analysis.csv" &= typFile &= help "Run the deriving analysis and output it to the specified file"
           , functionOpt = def &= explicit  &= name "f"  &= name "function" &= opt "./results/f_analysis.csv" &= typFile &= help "Run the function analysis and output it to the specified file"
           , uniplateOpt = def &= explicit  &= name "u"  &= name "uniplate" &= opt "./results/uni_analysis.csv" &= typFile &= help "Run the uniplate analysis and output it to the specified file"
           , hackageOpt = def &= explicit  &= name "h"  &= name "misc_hackage" &= opt "./results/misc_hackage.csv" &= typFile &= help "Print general analysis info or output it to the specified file"
           , dateOpt = def &= explicit  &= name "t" &= name "misc_date" &= opt "./results/misc_date.csv" &= typFile &= help "Get syb+uniplate date info and output it to the specified file (implies --misc_hackage)"
           , hackageDirOpt = def &= explicit  &= name "hackage_dir" &= opt "/tmp/hackage" &= typDir &= help "Set the hackage archive directory"
           , hackageLogOpt = def &= explicit  &= name "hackage_log" &= opt "./hackage.log" &= typFile &= help "Set the hackage archive log file"
           , cppOpt = def &= explicit &= name "c"  &= name "cpp" &= help "Set this flag to preprocess the hackage and yield an analysis with less failed-to-parse modules"
           } &= summary "HackageAnalysis Experimentation Project v1.0" &= program "run"



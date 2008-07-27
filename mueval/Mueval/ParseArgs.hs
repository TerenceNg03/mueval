module Mueval.ParseArgs (Options(..), interpreterOpts, getOptions) where

import Control.Monad (liftM)
import System.Console.GetOpt
import System.Environment (getArgs)

import qualified System.IO.UTF8 as UTF (putStr)
import qualified Codec.Binary.UTF8.String as Codec (decodeString)

import Mueval.Context (defaultModules)

-- | See the results of --help for information on what each option means.
data Options = Options
 { timeLimit :: Int
   , modules :: [String]
   , expression :: String
   , user :: String
   , printType :: Bool
   , extensions :: Bool
 } deriving Show

defaultOptions :: Options
defaultOptions = Options { expression = ""
                           , modules = defaultModules
                           , timeLimit = 5
                           , user = ""
                           , printType = False
                           , extensions = False }

options :: [OptDescr (Options -> Options)]
options = [Option ['p']     ["password"]
                      (ReqArg (\u opts -> opts {user = u}) "PASSWORD")
                      "The password for the mubot account. If this is set, mueval will attempt to setuid to the mubot user. This is optional, as it requires the mubot user to be set up properly. (Currently a null-op.)",
           Option ['t']     ["timelimit"]
                      (ReqArg (\t opts -> opts { timeLimit = (read t :: Int) }) "TIME")
                      "Time limit for compilation and evaluation",
           Option ['m']     ["module"]
                      (ReqArg (\m opts -> opts { modules = m:(modules opts) }) "MODULE")
                      "A module we should import functions from for evaluation. (Can be given multiple times.)",
           Option ['E']     ["Extensions"]
                      (NoArg (\opts -> opts { extensions = True}))
                      "Whether to enable the Glasgow extensions to Haskell '98. Defaults to false, but enabling is useful for QuickCheck.",
           Option ['e']     ["expression"]
                      (ReqArg (\e opts -> opts { expression = e}) "EXPRESSION")
                      "The expression to be evaluated.",
           Option ['i']     ["inferred-type"]
                      (NoArg (\opts -> opts { printType = True}))
                      "Whether to enable printing of inferred type and the expression (as Mueval sees it). Defaults to false." ]

interpreterOpts :: [String] -> IO (Options, [String])
interpreterOpts argv =
       case getOpt Permute options argv of
          (o,n,[]) -> return (foldl (flip id) defaultOptions o, n)
          (_,_,er) -> ioError $ userError (concat er ++ usageInfo header options)
      where header = "Usage: mueval [OPTION...] --expression EXPRESSION..."

-- | Just give us the end result options; this handles I/O and parsing for
-- us. Bonus points for handling UTF.
getOptions :: IO Options
getOptions = do input <- liftM (map Codec.decodeString) getArgs
                (opts,_) <- interpreterOpts $ input
                return opts
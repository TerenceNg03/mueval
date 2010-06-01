module Mueval.ArgsParse (Options(..), interpreterOpts, getOptions) where

import Control.Monad (liftM)
import System.Console.GetOpt

import qualified Codec.Binary.UTF8.String as Codec (decodeString)

import Mueval.Context (defaultModules)

-- | See the results of --help for information on what each option means.
data Options = Options
 { timeLimit :: Int
   , modules :: Maybe [String]
   , expression :: String
   , loadFile :: String
   , user :: String
   , printType :: Bool
   , extensions :: Bool
   , namedExtensions :: [String]
   , noImports :: Bool
   , rLimits :: Bool
   , help :: Bool
 } deriving Show

defaultOptions :: Options
defaultOptions = Options { expression = ""
                           , modules = Just defaultModules
                           , timeLimit = 5
                           , user = ""
                           , loadFile = ""
                           , printType = False
                           , extensions = False
                           , namedExtensions = []
                           , noImports = False
                           , rLimits = False
                           , help = False }

options :: [OptDescr (Options -> Options)]
options = [Option "p"     ["password"]
                      (ReqArg (\u opts -> opts {user = u}) "PASSWORD")
                      "The password for the mubot account. If this is set, mueval will attempt to setuid to the mubot user. This is optional, as it requires the mubot user to be set up properly. (Currently a null-op.)",
           Option "t"     ["timelimit"]
                      (ReqArg (\t opts -> opts { timeLimit = read t :: Int }) "TIME")
                      "Time limit for compilation and evaluation",

           Option "l"     ["loadfile"]
                      (ReqArg (\e opts -> opts { loadFile = e}) "FILE")
                      "A local file for Mueval to load, providing definitions. Contents are trusted! Do not put anything dubious in it!",

           Option "m"     ["module"]
                      (ReqArg (\m opts -> opts { modules = liftM (m:) (modules opts)}) "MODULE")
                      "A module we should import functions from for evaluation. (Can be given multiple times.)",
           Option "n"     ["noimports"]
                      (NoArg (\opts -> opts { noImports = True}))
                      "Whether to import any default modules, such as Prelude; this is useful if you are loading a file which, say, redefines Prelude operators.",
           Option "E"     ["Extensions"]
                      (NoArg (\opts -> opts { extensions = True}))
                      "Whether to enable the Glasgow extensions to Haskell '98. Defaults to false, but enabling is useful for QuickCheck.",
           Option "X"     ["extension"]
                      (ReqArg (\e opts -> opts { namedExtensions = e : namedExtensions opts }) "EXTENSION")
                      "Pass additional flags enabling extensions just like you would to ghc. Example: -XViewPatterns",
           Option "e"     ["expression"]
                      (ReqArg (\e opts -> opts { expression = e}) "EXPRESSION")
                      "The expression to be evaluated.",
           Option "i"     ["inferred-type"]
                      (NoArg (\opts -> opts { printType = True}))
                      "Whether to enable printing of inferred type and the expression (as Mueval sees it). Defaults to false.",
           Option "r"     ["rlimits"]
                      (NoArg (\opts -> opts { rLimits = True}))
                      "Enable resource limits (using POSIX rlimits). Mueval does not by default since rlimits are broken on many systems.",
           Option "h" ["help"]
                       (NoArg (\opts -> opts { help = True}))
                       "Prints out usage info."
          ]

interpreterOpts :: [String] -> (Options, [String])
interpreterOpts argv =
       let (o,n,ers) = getOpt Permute options argv in
       let msg = usageInfo header options in
       let opts = foldl (flip id) defaultOptions o in
       if help opts then error msg else
        if not (null ers) then error (concat ers ++ msg) else
        (opts, n)

header :: String
header = "Usage: mueval [OPTION...] --expression EXPRESSION..."

-- | Just give us the end result options; this parsing for
--   us. Bonus points for handling UTF.
getOptions :: [String] -> Options
getOptions = fst . interpreterOpts . map Codec.decodeString
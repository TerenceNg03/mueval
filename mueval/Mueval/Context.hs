module Mueval.Context (cleanModules, defaultModules, unsafe) where

import Data.List (elem, isInfixOf)

{- | Return true if the String contains anywhere in it any keywords associated
   with dangerous functions. Unfortunately, this blacklist leaks like a sieve
   and will return many false positives (eg. 'unsafed "id \"unsafed\""' will
   evaluate to True, even though the phrase \"unsafe\" appears only in a String). But it
   will at least catch naive and simplistic invocations of "unsafePerformIO",
   "inlinePerformIO", and "unsafeCoerce". -}
unsafe :: String -> Bool
unsafe = \z -> any (`isInfixOf` z) ["unsafe", "inlinePerform", "liftIO", "Coerce", "Foreign",
                                    "Typeable", "Array", "IOBase", "Handle", "ByteString",
                                    "Editline", "GLUT", "lock", "ObjectIO", "System.Time",
                                    "OpenGL", "Control.Concurrent", "System.Posix",
                                    "throw", "Dyn", "cache", "stdin", "stdout", "stderr"]

-- | Return false if any of the listed modules cannot be found in the whitelist.
cleanModules :: [String] -> Bool
cleanModules = and . map (`elem` safeModules)

{- | Modules which we should load by default. These are of course whitelisted.
   Specifically, we want the Prelude because otherwise things are horribly
   crippled; we want SimpleReflect so we can do neat things (for said neat
   things, see
   <http://twan.home.fmf.nl/blog/haskell/simple-reflection-of-expressions.details>);
   and we want ShowQ and ShowFun to neuter IO stuff even more. -}
defaultModules :: [String]
defaultModules = ["Prelude", "ShowQ", "ShowFun", "SimpleReflect"]

-- | Borrowed from Lambdabot, this is the whitelist of modules which should be
--   safe to import functions from.
safeModules :: [String]
safeModules = defaultModules ++ ["Control.Applicative",
               "Control.Arrow",
               "Control.Arrow.Operations",
               "Control.Arrow.Transformer",
               "Control.Arrow.Transformer.All",
               "Control.Monad",
               "Control.Monad.Cont",
               "Control.Monad.Error",
               "Control.Monad.Fix",
               "Control.Monad.Identity",
               "Control.Monad.Instances",
               "Control.Monad.RWS",
               "Control.Monad.Reader",
               "Control.Monad.ST",
               "Control.Monad.State",
               "Control.Monad.State",
               "Control.Monad.Writer",
               "Control.Parallel",
               "Control.Parallel.Strategies",
               "Data.Array",
               "Data.Bits",
               "Data.Bool",
               "Data.ByteString",
               "Data.ByteString.Char8",
               "Data.ByteString.Lazy",
               "Data.ByteString.Lazy.Char8",
               "Data.Char",
               "Data.Complex",
               "Data.Dynamic",
               "Data.Either",
               "Data.Eq",
               "Data.Fixed",
               "Data.Foldable",
               "Data.Function",
               "Data.Generics",
               "Data.Generics",
               "Data.Graph",
               "Data.Int",
               "Data.IntMap",
               "Data.IntSet",
               "Data.Ix",
               "Data.List",
               "Data.Map",
               "Data.Maybe",
               "Data.Monoid",
               "Data.Number.BigFloat",
               "Data.Number.CReal",
               "Data.Number.Dif",
               "Data.Number.Fixed",
               "Data.Number.Interval",
               "Data.Number.Natural",
               "Data.Number.Symbolic",
               "Data.Ord",
               "Data.Ratio",
               "Data.Sequence",
               "Data.Set",
               "Data.Traversable",
               "Data.Tree",
               "Data.Tuple",
               "Data.Typeable",
               "Data.Word",
               "Math.OEIS",
               "System.Random",
               "Test.QuickCheck",
               "Text.PrettyPrint.HughesPJ",
               "Text.Printf"]
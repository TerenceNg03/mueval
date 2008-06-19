module Mueval.Context (cleanModules, defaultModules) where

import Data.List (elem)

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
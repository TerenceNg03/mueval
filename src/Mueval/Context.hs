{-# LANGUAGE CPP #-}
module Mueval.Context (
  cleanModules,
  defaultModules,
  defaultPackages,
  qualifiedModules,
) where


-----------------------------------------------------------------------------

-- | Return false if any of the listed modules cannot be found in the whitelist.
cleanModules :: [String] -> Bool
cleanModules = all (`elem` defaultModules)

{- | Modules which we should load by default. These are of course whitelisted.
   Specifically, we want the Prelude because otherwise things are horribly
   crippled; we want SimpleReflect so we can do neat things (for said neat
   things, see
   <http://twan.home.fmf.nl/blog/haskell/simple-reflection-of-expressions.details>);
   and we want ShowFun to neuter IO stuff even more.
   The rest should be safe to import without clashes, according to the Lambdabot
   sources. -}
defaultModules :: [String]
defaultModules = ["Prelude",
                  "ShowFun",
                  "Debug.SimpleReflect",
                  "Data.Function",
                  "Control.Applicative",
                  "Control.Arrow",
                  "Control.Monad",
                  "Control.Monad.Cont",
#if __GLASGOW_HASKELL__ >= 710
                  "Control.Monad.Except",
#else
                  "Control.Monad.Error",
#endif
                  "Control.Monad.Fix",
                  "Control.Monad.Identity",
#if !MIN_VERSION_base(4,7,0)
                  "Control.Monad.Instances",
#endif
                  "Control.Monad.RWS",
                  "Control.Monad.Reader",
                  "Control.Monad.State",
                  "Control.Monad.State",
                  "Control.Monad.Writer",
                  "Data.Array",
                  "Data.Bits",
                  "Data.Bool",
                  "Data.Char",
                  "Data.Complex",
                  "Data.Dynamic",
                  "Data.Either",
                  "Data.Eq",
                  "Data.Fixed",
                  "Data.Graph",
                  "Data.Int",
                  "Data.Ix",
                  "Data.List",
                  "Data.Maybe",
                  "Data.Monoid",
{- -- Commented out because they are not necessarily available. If anyone misses
   -- them, perhaps we could look into forcing a dependency on them in the Cabal
   -- file or perhaps enable them via a CLI flag. For now, we'll stash them in a comment.
               "Control.Parallel",
               "Control.Parallel.Strategies",
               "Data.Number.BigFloat",
               "Data.Number.CReal",
               "Data.Number.Dif",
               "Data.Number.Fixed",
               "Data.Number.Interval",
               "Data.Number.Natural",
               "Data.Number.Symbolic",
               "Math.OEIS",
-}
               "Data.Ord",
               "Data.Ratio",
               "Data.Tree",
               "Data.Tuple",
               "Data.Typeable",
               "Data.Word",
               "System.Random",
               "Test.QuickCheck",
               "Text.PrettyPrint.HughesPJ",
               "Text.Printf"]

defaultPackages :: [String]
defaultPackages = [ "array"
                  , "base"
                  , "bytestring"
                  , "containers"
                  ]

{- | Borrowed from Lambdabot, this is the whitelist of modules which should be
   safe to import functions from, but which we don't want to import by
   default.
   FIXME: make these qualified imports. The GHC API & Hint currently do not
   support qualified imports.
   WARNING: You can import these with --module, certainly, but the onus is on
   the user to make sure they fully disambiguate function names; ie:

   > mueval  --module Data.Map -e "Prelude.map (+1) [1..100]"
-}
qualifiedModules :: [(String, Maybe String)]
qualifiedModules = [
--                ("Control.Arrow.Transformer", Just "AT"),
--                ("Control.Arrow.Transformer.All", Just "AT"),
               ("Data.ByteString", Just "BS"),
               ("Data.ByteString.Char8", Just "BSC"),
               ("Data.ByteString.Lazy", Just "BSL"),
               ("Data.ByteString.Lazy.Char8", Just "BSLC"),
               ("Data.Foldable", Just "Data.Foldable"),
--               ("Data.Generics", Just "Data.Generics"),
               ("Data.IntMap", Just "IM"),
               ("Data.IntSet", Just "IS"),
               ("Data.Map", Just "M"),
               ("Data.Sequence", Just "Data.Sequence"),
               ("Data.Set", Just "S"),
               ("Data.Traversable", Just "Data.Traversable") ]

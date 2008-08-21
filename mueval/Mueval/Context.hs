module Mueval.Context (cleanModules, defaultModules, unsafe, checkNames) where

import Data.List (elem, isInfixOf)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts (parseModule)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Parser (ParseResult(..))

import Data.Set (fromList, member)
import Data.Typeable (typeOf)
import Data.Generics (listify)

{- | Return true if the String contains anywhere in it any keywords associated
   with dangerous functions. Unfortunately, this blacklist leaks like a sieve
   and will return many false positives (eg. 'unsafed "id \"unsafed\""' will
   evaluate to True, even though the phrase \"unsafe\" appears only in a String). But it
   will at least catch naive and simplistic invocations of "unsafePerformIO",
   "inlinePerformIO", and "unsafeCoerce". -}
unsafe :: String -> Bool
unsafe = \z -> any (`isInfixOf` z) unsafeNames

unsafeNames :: [String]
unsafeNames = ["unsafe", "inlinePerform", "liftIO", "Coerce", "Foreign",
               "Typeable", "Array", "IOBase", "Handle", "ByteString",
               "Editline", "GLUT", "lock", "ObjectIO", "System.Time",
               "OpenGL", "Control.Concurrent", "System.Posix",
               "throw", "Dyn", "cache", "stdin", "stdout", "stderr"]


-- | A Nothing indicates that the expression is well-formed and passes the
-- blacklist; a Just "unsafe..." is exactly as it seems.
type Result = Maybe String

-- | This type indicates that haskell-src-exts simply couldn't parse the Haskell fragment.
type ParseError = String

{-
ghci> let e = ((\(Right a)->a) . parseHsExp $ "let x = (unsafePerformIO(print())`seq`42) in x")
ghci> listify ((==typeOf(undefined::HsName)) . typeOf) e :: [HsName]
[HsIdent "x",HsIdent "unsafePerformIO",HsIdent "print",HsIdent "seq",HsIdent "x"]
-}
-- | Parse as Haskell expression and analyze for unsafe expressions. Compared to
-- the primitive string munging of 'unsafe', this is the Right Thing.
-- "Right Nothing" is the only safe result.
-- FIXME: Experimental and probably doesn't work. Could use some clean up and
-- better type-fu.
checkNames :: String -> Either ParseError Result
checkNames s = case parseHsExp s of
                 Left err -> Left err
                 Right expr -> Right . untilM isRascal . fmap showHsName . allHsNamesIn $ expr
  where untilM :: (a -> Bool) -> [a] -> Maybe a
        untilM _ [] = Nothing
        untilM p (x:xs) = if p x
          then Just x else untilM p xs
        allHsNamesIn :: HsExp -> [HsName]
        allHsNamesIn = listify ((== typeOf (undefined :: HsName)) . typeOf)
        showHsName :: HsName -> String
        showHsName (HsIdent a) = a
        showHsName (HsSymbol a) = a
        isRascal :: String -> Bool
        isRascal = flip member (fromList unsafeNames)


-- | Return false if any of the listed modules cannot be found in the whitelist.
cleanModules :: [String] -> Bool
cleanModules = and . map (`elem` safeModules)

{- | Modules which we should load by default. These are of course whitelisted.
   Specifically, we want the Prelude because otherwise things are horribly
   crippled; we want SimpleReflect so we can do neat things (for said neat
   things, see
   <http://twan.home.fmf.nl/blog/haskell/simple-reflection-of-expressions.details>);
   and we want ShowQ and ShowFun to neuter IO stuff even more.
   The rest should be safe to import without clashes, according to the Lambdabot
   sources. -}
defaultModules :: [String]
defaultModules = ["Prelude", "ShowQ", "ShowFun", "SimpleReflect", "Data.Function",
               "Control.Applicative",
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
               "Data.Number.BigFloat",
               "Data.Number.CReal",
               "Data.Number.Dif",
               "Data.Number.Fixed",
               "Data.Number.Interval",
               "Data.Number.Natural",
               "Data.Number.Symbolic",
               "Data.Ord",
               "Data.Ratio",
               "Data.Tree",
               "Data.Tuple",
               "Data.Typeable",
               "Data.Word",
               "Math.OEIS",
               "System.Random",
               "Test.QuickCheck",
               "Text.PrettyPrint.HughesPJ",
               "Text.Printf"]

{- | Borrowed from Lambdabot, this is the whitelist of modules which should be
   safe to import functions from, but which we don't want to import by
   default.
   FIXME: make these qualified imports. The GHC API & Hint currently do not
   support qualified imports.
   WARNING: You can import these with --module, certainly, but the onus is on
   the user to make sure they fully disambiguate function names; ie:

   > mueval  --module Data.Map -e "Prelude.map (+1) [1..100]"
-}
safeModules :: [String]
safeModules = defaultModules ++ [
               "Control.Arrow",
               "Control.Arrow.Operations",
               "Control.Arrow.Transformer",
               "Control.Arrow.Transformer.All",
               "Data.ByteString",
               "Data.ByteString.Char8",
               "Data.ByteString.Lazy",
               "Data.ByteString.Lazy.Char8",
               "Data.Foldable",
               "Data.Generics",
               "Data.IntMap",
               "Data.IntSet",
               "Data.Map",
               "Data.Sequence",
               "Data.Set",
               "Data.Traversable"]

parseHsModule :: String -> Either String HsModule
parseHsModule s =
  case parseModule s of
    ParseOk m -> Right m
    ParseFailed loc e ->
      let line = srcLine loc - 1
      in Left (unlines [show line,show loc,e])

parseHsDecls :: String -> Either String [HsDecl]
parseHsDecls s =
  let s' = unlines [pprHsModule (emptyHsModule "Main"), s]
  in case parseModule s' of
      ParseOk m -> Right (moduleDecls m)
      ParseFailed loc e ->
        let line = srcLine loc - 1
        in Left (unlines [show line,show loc,e])

parseHsExp :: String -> Either String HsExp
parseHsExp s =
  case parseHsDecls ("main = ("++(filter (/='\n') s)++")") of
    Left err -> Left err
    Right xs ->
      case [ e | HsPatBind _ _ (HsUnGuardedRhs e) _ <- xs] of
        []    -> Left "invalid expression"
        (e:_) -> Right e

parseHsPat :: String -> Either String HsPat
parseHsPat s =
  case parseHsDecls ("(" ++ (filter (/='\n') s) ++ ")=()") of
    Left err -> Left err
    Right xs ->
      case [ p | HsPatBind _ p _ _ <- xs] of
        []    -> Left "invalid pattern"
        (p:_) -> Right p

pprHsModule :: HsModule -> String
pprHsModule = prettyPrint

moduleDecls :: HsModule -> [HsDecl]
moduleDecls (HsModule _ _ _ _ x) = x

mkModule :: String -> Module
mkModule = Module

emptySrcLoc :: SrcLoc
emptySrcLoc = (SrcLoc [] 0 0)

emptyHsModule :: String -> HsModule
emptyHsModule n =
    (HsModule
        emptySrcLoc
        (mkModule n)
        Nothing
        []
        [])

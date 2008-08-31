module Mueval.Context (
  cleanModules,
  defaultModules,
  unsafe,
  checkNames,
  CheckResult(..)
) where

import Data.List (elem, isInfixOf)
import Language.Haskell.Exts.Syntax
  (HsQName(..),HsName(..)
  ,Module(..),HsExp
  ,HsDecl(HsPatBind)
  ,HsRhs(HsUnGuardedRhs)
  ,HsSpecialCon(..),HsModule(..)
  ,SrcLoc(..))
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

-----------------------------------------------------------------------------

-- perhaps it would be better to have a whitelist
-- for functions to go along with the whitelist
-- for modules?
blacklisted :: String -> Bool
blacklisted = flip member . fromList $
  [ "unsafePerformIO", "inlinePerformIO"
  , "unsafeCoerce", "unsafeCoerce#", "unsafeInterleaveIO", "unsafeForeignPtrToPtr"
  , "throw", "throwDyn", "error", "unsafeInterleaveST", "unsafeIOToSTM"
  , "stdin", "stdout", "stderr", "unsafeIOToST", "readFile", "writeFile", "inline", "inlinePerformIO"]

type ParseError = String

-- | The @Maybe String@ is possibly a qualified module,
--  and the @String@ is an identifier name.
data CheckResult
  = CheckOk
  | CheckFailed
      (Maybe String)    -- maybe a module name (e.g. \"System.IO.Unsafe\")
        String          -- an identifier name (e.g. \"unsafePerformIO\")
  deriving (Eq,Ord,Show,Read)

isFailed :: CheckResult -> Bool
isFailed CheckOk = False
isFailed _       = True

-- Desired behaviour:
{-
> [m@ganon mueval]$ mueval -e 'MyLeetModule.uns4f3Perf0rmIO (print 42)'
mueval: Unsafe function(s) to use mentioned.

> [m@ganon mueval]$ mueval -e 'unsafePerformIO (print 42)'
mueval: Unsafe function(s) to use mentioned.

> [m@ganon mueval]$ mueval -e 'Foreign.blah 42'
mueval: Unsafe function(s) to use mentioned.

-}
{- Example GHCi usage of 'checknames'
ghci> checkNames "print \"unsafePerformIO\""
Right CheckOk
ghci> checkNames "System.IO.Unsafe.unsafePerformIO (print 42)"
Right (CheckFailed (Just "System.IO.Unsafe") "unsafePerformIO")
ghci> checkNames "MyLeetModule.uns4f3Perf0rmIO (print 42)"
Right (CheckFailed (Just "MyLeetModule") "uns4f3Perf0rmIO")
ghci> checkNames "MyLeetModule.uns4f3Perf0rmIO (print 42"
Left "2\nSrcLoc {srcFilename = \"<unknown>.hs\", srcLine = 3, srcColumn = 1}\nParse error\n"
-}

-- | Parse a Haskell expression and for each identifier, check that it is
--  not in the blacklist, and in the case of qualified identifiers, that
--  that module is present in the module whitelist. Compared to the
--  primitive string munging of @unsafe@, this is the Right Thing.
--  @Right CheckOk@ is the only safe result.
checkNames :: String -> Either ParseError CheckResult
checkNames s = case parseHsExp s of
                 Left e -> Left e
                 Right expr -> Right (maybe CheckOk id
                                      . find isFailed
                                        . fmap checkName
                                          . allNames $ expr)
  where find :: (a -> Bool) -> [a] -> Maybe a
        find _ [] = Nothing
        find p (x:xs) = if p x then Just x else find p xs
        allNames :: HsExp -> [(Maybe String, String)]
        allNames = fmap qNameToPair . qNames
          where qNames :: HsExp -> [HsQName]
                qNames = listify
                  ((== typeOf (undefined :: HsQName)) . typeOf)
                qNameToPair :: HsQName -> (Maybe String, String)
                qNameToPair (Qual modl name)
                  = (Just $ showModule modl, showHsName name)
                qNameToPair (UnQual name)
                  = (Nothing, showHsName name)
                qNameToPair (Special scon)
                  = (Nothing, showHsSpecialCon scon)
                showHsName :: HsName -> String
                showHsName (HsIdent a) = a
                showHsName (HsSymbol a) = a
                showHsSpecialCon :: HsSpecialCon -> String
                showHsSpecialCon HsUnitCon      = "()"
                showHsSpecialCon HsCons         = "(:)"
                showHsSpecialCon HsListCon      = "[]"
                showHsSpecialCon HsFunCon       = "(->)"
                showHsSpecialCon (HsTupleCon n) =
                  concat ["(",replicate (n-1) ',',")"]
        parseHsExp :: String -> Either String HsExp
        parseHsExp t =
          case parseHsDecls ("main = "++(filter (/='\n') t)) of
            Left err -> Left (err++t)
            Right xs ->
              case [ e | HsPatBind _ _ (HsUnGuardedRhs e) _ <- xs] of
                []    -> Left "invalid expression"
                (e:_) -> Right e
        parseHsDecls :: String -> Either String [HsDecl]
        parseHsDecls t =
          let t' = unlines [pprHsModule (emptyHsModule "Main"), t]
          in case parseModule t' of
              ParseOk m -> Right (moduleDecls m)
              ParseFailed loc e -> Left (unlines [show loc, e])
        pprHsModule :: HsModule -> String
        pprHsModule = prettyPrint
        moduleDecls :: HsModule -> [HsDecl]
        moduleDecls (HsModule _ _ _ _ x) = x
        mkModule :: String -> Module
        mkModule = Module
        emptySrcLoc :: SrcLoc
        emptySrcLoc = (SrcLoc [] 0 0)
        emptyHsModule :: String -> HsModule
        emptyHsModule n = (HsModule emptySrcLoc (mkModule n) Nothing [] [])
        showModule :: Module -> String
        showModule (Module a) = a
        checkName :: (Maybe String, String) -> CheckResult
        checkName (Nothing, name)
          | blacklisted name              = CheckFailed Nothing name
          | otherwise                     = CheckOk
        checkName (Just modl, name)
          | blacklisted name
            || (not . cleanModules) [modl] = CheckFailed (Just modl) name
          | otherwise                     = CheckOk

-----------------------------------------------------------------------------

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
               "Control.Arrow",
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
{- -- Commented out because they are not necessarily available. If anyone misses
   -- them, perhaps we could look into forcing a dependency on them in the Cabal
   -- file. For now, we'll let them be optional.

               "Data.Number.BigFloat",
               "Data.Number.CReal",
               "Data.Number.Dif",
               "Data.Number.Fixed",
               "Data.Number.Interval",
               "Data.Number.Natural",
               "Data.Number.Symbolic",
-}
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
               "Data.Traversable",
               "Data.Number.BigFloat",
               "Data.Number.CReal",
               "Data.Number.Dif",
               "Data.Number.Fixed",
               "Data.Number.Interval",
               "Data.Number.Natural",
               "Data.Number.Symbolic"]





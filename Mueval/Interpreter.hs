{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
-- TODO: suggest the convenience functions be put into Hint proper?
module Mueval.Interpreter where

import qualified Control.Exception.Extensible as E (evaluate,catch,SomeException(..))
import           Control.Monad (forM_,guard,mplus,unless,when)
import           Control.Monad.Trans (MonadIO)
import           Control.Monad.Writer (Any(..),runWriterT,tell)
import           Data.Char (isDigit)

import           System.Directory

import           System.Exit (exitFailure)
import           System.FilePath.Posix (takeBaseName)
import           System.IO (openTempFile)

import           Data.List

import           Language.Haskell.Interpreter (eval, set, reset, setImportsQ, loadModules, liftIO,
                                     installedModulesInScope, languageExtensions,
                                     typeOf, setTopLevelModules, runInterpreter, glasgowExtensions,
                                     OptionVal(..), Interpreter,
                                     InterpreterError(..),GhcError(..),
                                     Extension(UnknownExtension))
import           Language.Haskell.Interpreter.Unsafe (unsafeSetGhcOption)

import           Mueval.ArgsParse (Options(..))
import qualified Mueval.Resources as MR (limitResources)
import qualified Mueval.Context as MC (qualifiedModules)

readExt :: String -> Extension
readExt s = case reads s of
  [(e,[])] -> e
  _        -> UnknownExtension s

{- | The actual calling of Hint functionality. The heart of this just calls
   'eval', but we do so much more - we disable Haskell extensions,
   hide all packages, make sure one cannot call unimported
   functions, typecheck, set resource limits for this
   thread, and do some error handling. -}
interpreter :: Options -> Interpreter (String,String,String)
interpreter Options { extensions = exts, namedExtensions = nexts,
                      rLimits = rlimits,
                      typeOnly = noEval,
                      loadFile = load, expression = expr,
                      packageTrust = trust,
                      trustedPackages = trustPkgs,
                      modules = m } = do
                                  let lexts = (guard exts >> glasgowExtensions) ++ map readExt nexts
                                  -- Explicitly adding ImplicitPrelude because of
                                  -- http://darcsden.com/jcpetruzza/hint/issue/1
                                  unless (null lexts) $ set [languageExtensions := (UnknownExtension "ImplicitPrelude" : lexts)]
                                  when trust $ do
                                    unsafeSetGhcOption "-fpackage-trust"
                                    forM_ (trustPkgs >>= words) $ \pkg ->
                                      unsafeSetGhcOption ("-trust " ++ pkg)

                                  reset -- Make sure nothing is available
                                  set [installedModulesInScope := False]

                                  -- if we're given a file of definitions, we need to first copy it to a temporary file in /tmp (cpload),
                                  -- then tell Hint to parse/read it, then extract the 'module name' of the file,
                                  -- and tell Hint to expose the module into memory; then we need to store the temporary file's filepath
                                  -- so we can try to clean up after ourselves later.
                                  lfl' <- if (load /= "") then (do { lfl <- liftIO (cpload load);
                                                                     loadModules [lfl];
                                                                     -- We need to mangle the String to
                                                                     -- turn a filename into a module.
                                                                     setTopLevelModules [takeBaseName load];
                                                                     return lfl }) else (return "")

                                  liftIO $ MR.limitResources rlimits

                                  case m of
                                    Nothing -> return ()
                                    Just ms -> do let unqualModules =  zip ms (repeat Nothing)
                                                  setImportsQ (unqualModules ++ MC.qualifiedModules)

                                  -- clean up our tmp file here; must be *after* setImportsQ
                                  when (load /= "") $ liftIO (removeFile lfl')

                                  -- we don't deliberately don't check if the expression typechecks
                                  -- this way we get an "InterpreterError" we can display
                                  etype <- typeOf expr
                                  result <- if noEval
                                               then return ""
                                               else eval expr

                                  return (expr, etype, result)

-- | Wrapper around 'interpreter'; supplies a fresh GHC API session and
-- error-handling. The arguments are largely passed on, and the results lightly parsed.
interpreterSession :: Options -> IO ()
interpreterSession opts = do r <- runInterpreter (interpreter opts)
                             case r of
                                 Left err -> printInterpreterError err
                                 Right (e,et,val) -> do when (printType opts)
                                                             (sayIO e >> sayIOOneLine et)
                                                        sayIO val
  where sayIOOneLine = sayIO . unwords . words

-- | Given a filepath (containing function definitions), copy it to a temporary file and change directory to it, returning the new filepath.
cpload :: FilePath -> IO FilePath
cpload definitions = do
                tmpdir <- getTemporaryDirectory
                (tempfile,_) <- System.IO.openTempFile tmpdir "mueval.hs"
                liftIO $ copyFile definitions tempfile
                setCurrentDirectory tmpdir -- will at least mess up relative links
                return tempfile

---------------------------------
-- Handling and outputting results
-- TODO: this whole section is a hack

-- | Print the String (presumably the result
-- of interpreting something), but only print the first 1024 characters to avoid
-- flooding. Lambdabot has a similar limit.
sayIO :: String -> IO ()
sayIO str = do (out,b) <- render 1024 str
               putStrLn out
               when b exitFailure

-- | Oh no, something has gone wrong. If it's a compilation error pretty print
-- the first 1024 chars of it and throw an "ExitException"
-- otherwise rethrow the exception in String form.
printInterpreterError :: InterpreterError -> IO ()
printInterpreterError (WontCompile errors) =
    -- if we get a compilation error we print it directly to avoid \"mueval: ...\"
    -- maybe it should go to stderr?
    do sayIO $ concatMap (dropLinePosition . errMsg) errors
       exitFailure
    where
      -- each error starts with the line position, which is uninteresting
      dropLinePosition e
          | Just s <- parseErr e =  s
          | otherwise = e -- if the parse fails we fallback on printing the whole error
      parseErr e = do s <- stripPrefix "<interactive>:" e
                      skipSpaces =<< (skipNumber =<< skipNumber s)
      skip x (y:xs) | x == y = Just xs
                    | otherwise = Nothing
      skip _ _ = Nothing
      skipNumber = skip ':' . dropWhile isDigit
      skipSpaces xs = let xs' = dropWhile (==' ') xs
                      in skip '\n' xs' `mplus` return xs'

-- other exceptions indicate some problem in Mueval or the environment,
-- so we rethrow them for debugging purposes
printInterpreterError other = error (show other)

-- Constant
exceptionMsg :: String
exceptionMsg = "*Exception: "

-- | Renders the input String including its exceptions using @exceptionMsg@
render :: (Control.Monad.Trans.MonadIO m, Functor m)
          => Int -- ^ max number of characters to include
          -> String -- ^ input
          -> m (String, Bool) -- ^ ( output, @True@ if we found an exception )
render i xs =
    do (out,Any b) <- runWriterT $ render' i (toStream xs)
       return (out,b)
    where
      render' n _ | n <= 0 = return ""
      render' n s = render'' n =<< liftIO s

      render'' _ End = return ""
      render'' n (Cons x s) = fmap (x:) $ render' (n-1) s
      render'' n (Exception s) = do
        tell (Any True)
        fmap (take n exceptionMsg ++) $ render' (n - length exceptionMsg) s

data Stream = Cons Char (IO Stream) | Exception (IO Stream) | End

toStream :: String -> IO Stream
toStream str = E.evaluate (uncons str) `E.catch`
                \(E.SomeException e) -> return . Exception . toStream . show $ e
    where uncons [] = End
          uncons (x:xs) = x `seq` Cons x (toStream xs)

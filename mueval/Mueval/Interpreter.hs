-- TODO: suggest the convenience functions be put into Hint proper?
module Mueval.Interpreter where

import Control.Monad (when)
import qualified Control.Exception (catch)
import Control.Monad.Trans (liftIO)
import System.Directory (copyFile, makeRelativeToCurrentDirectory, removeFile)

import Language.Haskell.Interpreter.GHC (eval, newSession, reset, setImports, loadModules,
                                         setOptimizations, setUseLanguageExtensions, setInstalledModsAreInScopeQualified,
                                         typeChecks, typeOf, withSession, setTopLevelModules,
                                         Interpreter, InterpreterError, ModuleName, Optimizations(All))


import qualified Codec.Binary.UTF8.String as Codec (decodeString)
import qualified System.IO.UTF8 as UTF (putStr)

import qualified Mueval.Resources (limitResources)

-- | From inside the Interpreter monad, print the String (presumably the result
-- of interpreting something), but only print the first 1024 characters to avoid
-- flooding. Lambdabot has a similar limit.
say :: String -> Interpreter ()
say = liftIO . UTF.putStr . Codec.decodeString . take 1024

-- | Oh no, something has gone wrong. Call 'error' and then, as with 'say',
-- print out a maximum of 1024 characters.
printInterpreterError :: InterpreterError -> IO ()
printInterpreterError = error . take 1024 . ("Oops... " ++) . show

{- | The actual calling of Hint functionality. The heart of this just calls
   'eval', but we do so much more - we disable Haskell extensions, turn on
   optimizations, hide all packages, make sure one cannot call unimported
   functions, typecheck (and optionally print it), set resource limits for this
   thread, and do some error handling. -}
interpreter :: Bool -> Bool -> [ModuleName] -> String -> String -> Interpreter ()
interpreter prt exts modules lfl expr = do
                                  setUseLanguageExtensions exts -- False by default

                                  setOptimizations All -- Maybe optimization will make
                                                       -- more programs
                                                       -- terminate.

                                  reset -- Make sure nothing is available
                                  setInstalledModsAreInScopeQualified False

                                  let doload = if lfl == "" then False else True

                                  when doload (liftIO $ mvload lfl)

                                  liftIO Mueval.Resources.limitResources

                                  when doload $ do loadModules [lfl]
                                                   -- We need to mangle the String to
                                                   -- turn a filename into a
                                                   -- module
                                                   setTopLevelModules [(takeWhile (/='.') lfl)]

                                  setImports modules

                                  when prt (say $ expr ++ "\n")

                                  checks <- typeChecks expr

                                  if checks then do
                                              if prt then do say =<< typeOf expr
                                                             say "\n"
                                               else return ()
                                              result <- eval expr
                                              say $ show result ++ "\n"
                                    else error "Expression did not type check."

-- | Wrapper around 'interpreter'; supplies a fresh GHC API session and
-- error-handling. The arguments are simply passed on.
interpreterSession :: Bool -- ^ Whether to print inferred type
                   -> Bool -- ^ Whether to use GHC extensions
                   -> [ModuleName] -- ^ A list of modules we wish to be visible
                   -> String -- ^ A local file from which to grab definitions; an
                            -- empty string is treated as no file.
                   -> String -- ^ The string to be interpreted as a Haskell expression
                   -> IO ()  -- ^ No real result, since printing is done deeper in
                            -- the stack.
interpreterSession prt exts mds lfl expr = Control.Exception.catch
                                  (newSession >>= (flip withSession) (interpreter prt exts mds lfl expr))
                                  (\_ -> do case lfl of
                                             "" -> return ()
                                             l  -> do canonfile <- (makeRelativeToCurrentDirectory l)
                                                      removeFile ("/tmp/" ++ canonfile)
                                            error "Expression did not compile.")

mvload :: FilePath -> IO ()
mvload lfl = do canonfile <- (makeRelativeToCurrentDirectory lfl)
                liftIO $ copyFile canonfile ("/tmp/" ++ canonfile)
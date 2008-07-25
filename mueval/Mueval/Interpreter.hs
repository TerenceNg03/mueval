-- TODO: suggest the convenience functions be put into Hint proper?
module Mueval.Interpreter where

import Control.Monad.Trans (liftIO)
import qualified Control.Exception (catch)

import Language.Haskell.Interpreter.GHC (eval, newSession, reset, setImports,
                                         setOptimizations, setUseLanguageExtensions, setInstalledModsAreInScopeQualified,
                                                         typeChecks, typeOf, withSession,
                                         Interpreter, InterpreterError, ModuleName, Optimizations(All))

import qualified Mueval.Resources (limitResources)

-- | From inside the Interpreter monad, print the String (presumably the result
-- of interpreting something), but only print the first 1024 characters to avoid
-- flooding. Lambdabot has a similar limit.
say :: String -> Interpreter ()
say = liftIO . putStr . take 1024

-- | Oh no, something has gone wrong. Call 'error' and then, as with 'say',
-- print out a maximum of 1024 characters.
printInterpreterError :: InterpreterError -> IO ()
printInterpreterError = error . take 1024 . ("Oops... " ++) . show

{- | The actual calling of Hint functionality. The heart of this just calls
   'eval', but we do so much more - we disable Haskell extensions, turn on
   optimizations, hide all packages, make sure one cannot call unimported
   functions, typecheck (and optionally print it), set resource limits for this
   thread, and do some error handling. -}
interpreter :: Bool -> [ModuleName] -> String -> Interpreter ()
interpreter prt modules expr = do
                                  setUseLanguageExtensions False -- Don't trust the
                                                                 -- extensions
                                  setOptimizations All -- Maybe optimization will make
                                                       -- more programs terminate.
                                  reset -- Make sure nothing is available
                                  setInstalledModsAreInScopeQualified False
                                  setImports modules

                                  checks <- typeChecks expr
                                  liftIO Mueval.Resources.limitResources
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
                   -> [ModuleName] -- ^ A list of modules we wish to be visible
                   -> String -- ^ The string to be interpreted as a Haskell expression
                   -> IO ()  -- ^ No real result, since printing is done deeper in
                            -- the stack.
interpreterSession prt mds expr = Control.Exception.catch
                                  (newSession >>= (flip withSession) (interpreter prt mds expr))
                                  (\_ -> error "Expression did not compile.")
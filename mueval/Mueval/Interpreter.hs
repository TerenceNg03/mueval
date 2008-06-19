-- TODO: suggest the convenience functions be put into Hint proper?
module Mueval.Interpreter (interpreterSession, printInterpreterError, ModuleName) where

import Language.Haskell.Interpreter.GHC (eval, newSession, reset, setImports,
                                         setOptimizations, setUseLanguageExtensions, typeChecks,
                                         typeOf, withSession,
                                         Interpreter, InterpreterError, ModuleName, Optimizations(All))

import Control.Monad.Trans (liftIO)
import System.Exit (exitWith, ExitCode(ExitFailure))

say :: String -> Interpreter ()
say = liftIO . putStr . take 1024

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError e = do putStrLn $ take 1024 $ "Oops... " ++ (show e)
                             (exitWith $ ExitFailure 1)

interpreter :: Bool -> [ModuleName] -> String -> Interpreter ()
interpreter prt modules expr = do setUseLanguageExtensions False -- Don't trust the
                                                                 -- extensions
                                  setOptimizations All -- Maybe optimization will make
                                                       -- more programs terminate.
                                  reset -- Make sure nothing is available
                                  setImports modules
                                  checks <- typeChecks expr
                                  if checks then do
                                              if prt then do say =<< typeOf expr
                                                             say "\n"
                                               else return ()
                                              result <- eval expr
                                              say $ show result ++ "\n"
                                    else error "Expression does not type check."

interpreterSession :: Bool -> [ModuleName] -> String -> IO ()
interpreterSession prt mds expr = newSession >>= (flip withSession) (interpreter prt mds expr)
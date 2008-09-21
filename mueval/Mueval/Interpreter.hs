{-# LANGUAGE PatternGuards #-}
-- TODO: suggest the convenience functions be put into Hint proper?
module Mueval.Interpreter where

import Control.Monad (when,mplus)
import Control.Monad.Trans (liftIO)
import System.Directory (copyFile, makeRelativeToCurrentDirectory, removeFile)
import System.Exit (exitFailure)
import System.FilePath.Posix (takeFileName)
import qualified Control.Exception as E (bracket,catchDyn,evaluate,catch)

import Language.Haskell.Interpreter.GHC (eval, newSession, reset, setImports, loadModules,
                                         setOptimizations, setUseLanguageExtensions, setInstalledModsAreInScopeQualified,
                                         typeOf, withSession, setTopLevelModules,
                                         Interpreter, InterpreterError(..),GhcError(..), ModuleName, Optimizations(All))
import qualified Mueval.Resources (limitResources)
import qualified System.IO.UTF8 as UTF (putStrLn)
import Control.Monad.Writer (Any(..),runWriterT,tell)
import Data.List (stripPrefix)
import Data.Char (isDigit)

{- | The actual calling of Hint functionality. The heart of this just calls
   'eval', but we do so much more - we disable Haskell extensions, turn on
   optimizations, hide all packages, make sure one cannot call unimported
   functions, typecheck (and optionally print it), set resource limits for this
   thread, and do some error handling. -}
interpreter :: Bool -> Bool -> Bool -> Maybe [ModuleName] -> String -> String -> Interpreter ()
interpreter prt exts rlimits modules lfl expr = do
                                  setUseLanguageExtensions exts -- False by default

                                  setOptimizations All -- Maybe optimization will make
                                                       -- more programs
                                                       -- terminate.

                                  reset -- Make sure nothing is available
                                  setInstalledModsAreInScopeQualified False

                                  let doload = if lfl == ""
                                                then False else True

                                  when doload (liftIO $ mvload lfl)

                                  liftIO $ Mueval.Resources.limitResources rlimits

                                  when doload $ do
                                                   let lfl' = takeFileName lfl
                                                   loadModules [lfl']
                                                   -- We need to mangle the String to
                                                   -- turn a filename into a
                                                   -- module
                                                   setTopLevelModules [(takeWhile (/='.') lfl')]

                                  case modules of
                                    Nothing -> return ()
                                    Just ms -> setImports ms

                                  when prt $ say expr
                                  -- we don't check if the expression typechecks
                                  -- this way we get an "InterpreterError" we can display
                                  when prt $ say =<< typeOf expr

                                  result <- eval expr

                                  say $ result

-- | Wrapper around 'interpreter'; supplies a fresh GHC API session and
-- error-handling. The arguments are simply passed on.
interpreterSession :: Bool -- ^ Whether to print inferred type
                   -> Bool -- ^ Whether to use GHC extensions
                   -> Bool -- ^ Whether to use rlimits
                   -> Maybe [ModuleName] -- ^ A list of modules we wish to be visible
                   -> String -- ^ A local file from which to grab definitions; an
                            -- empty string is treated as no file.
                   -> String -- ^ The string to be interpreted as a Haskell expression
                   -> IO ()  -- ^ No real result, since printing is done deeper in
                            -- the stack.
interpreterSession prt exts rls mds lfl expr = E.bracket newSession cleanTmpFile $ \session ->
                                  withSession session (interpreter prt exts rls mds lfl expr)
                                  `E.catchDyn` printInterpreterError
    where
      cleanTmpFile _ = case lfl of
                         "" -> return ()
                         l  -> do canonfile <- makeRelativeToCurrentDirectory l
                                  removeFile $ "/tmp/" ++ takeFileName canonfile


mvload :: FilePath -> IO ()
mvload lfl = do canonfile <- (makeRelativeToCurrentDirectory lfl)
                liftIO $ copyFile canonfile ("/tmp/" ++ (takeFileName canonfile))

---------------------------------
-- Handling and outputting results

-- | From inside the Interpreter monad, print the String (presumably the result
-- of interpreting something), but only print the first 1024 characters to avoid
-- flooding. Lambdabot has a similar limit.
say :: String -> Interpreter ()
say = liftIO . sayIO

sayIO :: String -> IO ()
sayIO str = do (out,b) <- render 1024 str
               UTF.putStrLn out
               when b $ exitFailure

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
                      skipSpaces =<<(skipNumber =<< skipNumber s)
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
exceptionMsg = "* Exception: "

-- | renders the input String including its exceptions using @exceptionMsg@
render :: Int -- ^ max number of characters to include
       -> String -- ^ input 
       -> IO (String, Bool) -- ^ ( output, @True@ if we found an exception )
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
        fmap (take n exceptionMsg ++) $
             render' (n-length exceptionMsg) s

data Stream = Cons Char (IO Stream) | Exception (IO Stream) | End

toStream :: String -> IO Stream
toStream str = (E.evaluate (uncons str)) `E.catch` \e -> return $ Exception $ toStream (show e)

    where uncons [] = End
          uncons (x:xs) = x `seq` Cons x (toStream xs)


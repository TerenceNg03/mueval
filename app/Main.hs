-- TODO:
-- Need to add user switching. Perhaps using seteuid and setegid? See
-- <http://www.opengroup.org/onlinepubs/009695399/functions/seteuid.html> &
-- <http://www.opengroup.org/onlinepubs/009695399/functions/setegid.html>
module Main (main) where

import Mueval.Parallel
import Mueval.ArgsParse (interpreterOpts)
import System.Environment
import System.Exit

main :: IO ()
main = do args <- getArgs
          -- force parse errors in main's thread
          case interpreterOpts args of
              Left (n,s) -> putStrLn s >> if n then exitSuccess else exitFailure
              Right opts -> forkedMain $! opts

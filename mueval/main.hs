-- TODO:
-- Need to add user switching. Perhaps using seteuid and setegid? See
-- <http://www.opengroup.org/onlinepubs/009695399/functions/seteuid.html> &
-- <http://www.opengroup.org/onlinepubs/009695399/functions/setegid.html>
module Main (main) where

import Mueval.Parallel
import Mueval.ArgsParse (getOptions)
import System.Environment

main :: IO ()
main = do args <- getArgs
          -- force parse errors in main's thread
          forkedMain $! getOptions args

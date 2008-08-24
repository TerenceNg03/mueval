-- TODO:
-- Need to add user switching. Perhaps using seteuid and setegid? See
-- <http://www.opengroup.org/onlinepubs/009695399/functions/seteuid.html> &
-- <http://www.opengroup.org/onlinepubs/009695399/functions/setegid.html>
module Main (main) where

import Mueval.Parallel
import Mueval.ArgsParse (getOptions, Options(..))
import qualified Mueval.Context as C
  (cleanModules, checkNames, CheckResult(CheckOk,CheckFailed))

main :: IO ()
main = do opts <- getOptions
          doIfSafe opts forkedMain

-- We don't keep this in one of the other modules, because it's policy; other
-- similar programs may not care.
doIfSafe :: Options -> (Options -> t t1) -> t t1
doIfSafe opts f =
  case C.cleanModules (modules opts) of
    True -> case C.checkNames (expression opts) of
              Left e -> error e
              Right C.CheckOk -> f opts
              Right (C.CheckFailed _ _) -> error
                "Unsafe function(s) to use mentioned."
    _ -> error "Unknown or untrusted module supplied! Aborting."


-- TODO:
-- Need to add user switching. Perhaps using seteuid and setegid? See
-- <http://www.opengroup.org/onlinepubs/009695399/functions/seteuid.html> &
-- <http://www.opengroup.org/onlinepubs/009695399/functions/setegid.html>
module Main (main) where


import qualified Mueval.Context (cleanModules, unsafe)
import Mueval.ParseArgs (getOptions, Options(..))
import Mueval.Concurrent

main :: IO ()
main = do opts <- getOptions
          doIfSafe opts forkedMain

-- We don't keep this in one of the other modules, because it's policy; other
-- similar programs may not care.
doIfSafe :: Options -> (Options -> t t1) -> t t1
doIfSafe opts f = if (Mueval.Context.cleanModules $
                            modules opts) then do
                                            if (not $ Mueval.Context.unsafe $ expression opts) then
                                               f opts
                                             else error "Unsafe functions to use mentioned."
                  else error "Unknown or untrusted module supplied! Aborting."


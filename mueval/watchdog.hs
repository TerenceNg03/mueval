-- | This implements a watchdog process. It calls mueval with all the
--   user-specified arguments, sleeps, and then if mueval is still running
--   kills it.
--   Even an out-of-control mueval will have trouble avoiding 'terminateProcess'.
--   Note that it's too difficult to parse the user arguments to get the timeout,
--   so we specify it as a constant which is a little more generous than the default.
module Main where

import System.Environment
import Control.Concurrent
import System.Process
import System.Exit

main :: IO ()
main = do args <- getArgs
          hdl <- runProcess "mueval-core" args Nothing Nothing Nothing Nothing Nothing
          forkIO (do threadDelay (7 * 700000)
                     status <- getProcessExitCode hdl
                     case status of 
                         Nothing -> terminateProcess hdl >> exitWith (ExitFailure 1)
                         Just a -> exitWith a)
          stat <- waitForProcess hdl
          exitWith stat

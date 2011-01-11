-- | This implements a watchdog process. It calls mueval with all the
--   user-specified arguments, sleeps, and then if mueval is still running
--   kills it.
--   Even an out-of-control mueval will have trouble avoiding 'terminateProcess'.
--   Note that it's too difficult to parse the user arguments to get the timeout,
--   so we specify it as a constant which is a little more generous than the default.
module Main where

import Control.Concurrent (forkIO, threadDelay)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.Posix.Signals (signalProcess)
import System.Process (getProcessExitCode, runProcess, terminateProcess, waitForProcess)
import System.Process.Internals (withProcessHandle, ProcessHandle__(OpenHandle))

main :: IO ()
main = do args <- getArgs
          hdl <- runProcess "mueval-core" args Nothing Nothing Nothing Nothing Nothing
          _ <- forkIO $ do
                     threadDelay (7 * 700000)
                     status <- getProcessExitCode hdl
                     case status of 
                         Nothing -> do terminateProcess hdl
                                       _ <- withProcessHandle hdl (\x -> case x of 
                                                                      OpenHandle pid -> signalProcess 9 pid >> return (undefined, undefined)
                                                                      _ -> return (undefined,undefined))
                                       exitWith (ExitFailure 1)
                         Just a -> exitWith a
          stat <- waitForProcess hdl
          exitWith stat

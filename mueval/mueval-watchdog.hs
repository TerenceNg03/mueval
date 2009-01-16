-- | This implements a watchdog process. It sleeps for a specified time, 
--   and then it wakes up and calls kill -9 on the specified process.
--   This is intended for use with mueval - mueval will call this, pass 
--   the watchdog mueval's process ID, and then if mueval goes crazy and
--   the internal watchdog thread can't/won't handle it, this process remains
--   incorruptible. Even a out-of-control process can't ignore a kill -9 easily.
module Main where

import System.Environment
import Control.Concurrent
import System.Posix.Signals

main :: IO ()
main = do args <- getArgs
          let pid = read (args !! 0)
          let tout = read (args !! 1) :: Int
          threadDelay (tout * 700000)
          signalProcess killProcess pid
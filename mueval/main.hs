-- TODO: Currently we usually exit successfully even when there was a
-- problem. Need to sort out the exit code business.
-- Need to add user switching. Perhaps using seteuid and setegid? See
-- <http://www.opengroup.org/onlinepubs/009695399/functions/seteuid.html> &
-- <http://www.opengroup.org/onlinepubs/009695399/functions/setegid.html>
module Main (main) where

import Control.Concurrent   (forkIO, killThread, myThreadId, threadDelay, throwTo, yield, ThreadId)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, MVar)
import Control.Exception (catchDyn, Exception(ErrorCall))
import System.Environment (getArgs)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Posix.Signals (sigXCPU, installHandler, Handler(CatchOnce))

import qualified Mueval.Context (cleanModules, unsafed)
import Mueval.Interpreter
import Mueval.ParseArgs

main :: IO ()
main = do input <- getArgs
          (opts,_) <- interpreterOpts input
          if (Mueval.Context.cleanModules $ modules opts) then do
              if (not $ Mueval.Context.unsafed $ expression opts) then do
                                               mvar <- newEmptyMVar

                                               myThreadId >>= watchDog (timeLimit opts)

                                               forkIO $ forkedMain (mvar) opts
                                               takeMVar mvar -- block until a ErrorCall or the forkedMain succeeds

                                               return ()
               else error "Unsafe functions to use mentioned."
           else error "Unknown or untrusted module supplied! Aborting."

-- Set a watchdog, and then evaluate.
forkedMain :: MVar [Char] -> Options -> IO ()
forkedMain mvar opts = do
  -- This *should* be redundant with the previous watchDog,
  -- but maybe not.

  myThreadId >>= watchDog tout

  hSetBuffering stdout NoBuffering

  -- Our modules and expression are set up. Let's do stuff.
  interpreterSession typeprint mdls expr `catchDyn` (printInterpreterError)
  putMVar mvar "Done."
          where mdls = modules opts
                expr = expression opts
                tout = timeLimit opts
                typeprint = printType opts

-- | Fork off a thread which will sleep and kill off another thread at some point.
watchDog :: Int -> ThreadId -> IO ()
watchDog tout tid = do installHandler sigXCPU
                                          (CatchOnce
                                           $ throwTo tid $ ErrorCall "Time limit exceeded.") Nothing
                       forkIO $ do threadDelay (tout * 1000000)
                                   -- Time's up. It's a good day to die.
                                   throwTo tid (ErrorCall "Time limit exceeded")
                                   yield -- give the other thread a chance
                                   killThread tid -- Die now, srsly.
                                   error "Time expired"
                       return ()
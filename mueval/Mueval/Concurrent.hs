module Mueval.Concurrent where

import Control.Concurrent   (forkIO, killThread, myThreadId, threadDelay, throwTo, yield, ThreadId)
import System.Posix.Signals (sigXCPU, installHandler, Handler(CatchOnce))
import Control.Exception (catchDyn, Exception(ErrorCall))
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

import Mueval.Interpreter
import Mueval.ParseArgs

-- | Fork off a thread which will sleep and then kill off the specified thread.
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
                       return () -- Never reached. Either we error out in
                                 -- watchDog, or the evaluation thread finishes.

-- | Set a 'watchDog' on this thread, and then continue on with whatever.
forkedMain :: Options -> IO ()
forkedMain opts = do
  mvar <- newEmptyMVar

  myThreadId >>= watchDog tout

  hSetBuffering stdout NoBuffering

  -- Our modules and expression are set up. Let's do stuff.
  forkIO (interpreterSession typeprint mdls expr `catchDyn` (printInterpreterError) >> putMVar mvar "Done.")

  takeMVar mvar -- block until ErrorCall, or forkedMain succeeds
  return ()
          where mdls = modules opts
                expr = expression opts
                tout = timeLimit opts
                typeprint = printType opts

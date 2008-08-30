module Mueval.Parallel where

import Prelude hiding (catch)
import Control.Concurrent   (forkIO, killThread, myThreadId, threadDelay, throwTo, yield, ThreadId)
import System.Posix.Signals (sigXCPU, installHandler, Handler(CatchOnce))
import Control.Exception (Exception(ErrorCall),catch)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, MVar)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

import Mueval.Interpreter
import Mueval.ArgsParse

-- | Fork off a thread which will sleep and then kill off the specified thread.
watchDog :: Int -> ThreadId -> IO ()
watchDog tout tid = do installHandler sigXCPU
                                          (CatchOnce
                                           $ throwTo tid $ ErrorCall "Time limit exceeded.") Nothing
                       forkIO $ do threadDelay (tout * 700000)
                                   -- Time's up. It's a good day to die.
                                   throwTo tid (ErrorCall "Time limit exceeded")
                                   yield -- give the other thread a chance
                                   killThread tid -- Die now, srsly.
                                   error "Time expired"
                       return () -- Never reached. Either we error out in
                                 -- watchDog, or the evaluation thread finishes.

-- | A basic blocking operation.
block :: (t -> MVar a -> IO t1) -> t -> IO a
block f opts = do  mvar <- newEmptyMVar
                   f opts mvar
                   takeMVar mvar -- block until ErrorCall, or forkedMain succeeds

-- | Using MVars, block on forkedMain' until it finishes.
forkedMain :: Options -> IO ()
forkedMain opts = block forkedMain' opts >> return ()

-- | Set a 'watchDog' on this thread, and then continue on with whatever.
forkedMain' :: Options -> MVar [Char] -> IO ThreadId
forkedMain' opts mvar = do mainId <- myThreadId
                           watchDog tout mainId
                           hSetBuffering stdout NoBuffering

                      -- Our modules and expression are set up. Let's do stuff.
                           forkIO $ (interpreterSession typeprint extend rls mdls fls expr
                                                            >> putMVar mvar "Done.")
                                      `catch` throwTo mainId -- bounce exceptions to the main thread,
                                                             -- so they are reliably printed out
          where mdls = if impq then Nothing else Just (modules opts)
                expr = expression opts
                tout = timeLimit opts
                typeprint = printType opts
                extend = extensions opts
                fls = loadFile opts
                impq = noimports opts
                rls = rlimits opts
module Mueval.Parallel (runMueval) where

import Control.Monad (void)

import Mueval.ArgsParse (Options (modules, noImports, timeLimit))
import Mueval.Interpreter (interpreterSession)
import System.Timeout (timeout)

runMueval :: Options -> IO ()
runMueval opts =
    let time = timeLimit opts * 1000000
        checkImport x = if noImports x then x{modules = Nothing} else x
     in void $ timeout time $ interpreterSession (checkImport opts)

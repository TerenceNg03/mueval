module Mueval.Resources (limitResources) where

import Control.Monad (when)
import System.Posix.Process (nice)
import System.Posix.Resource -- (Resource(..), ResourceLimits, setResourceLimit)

-- | Pull together several methods of reducing priority and easy access to resources:
--  'nice', and the rlimit bindings,
--  If called with False, 'limitResources' will not use POSIX rlimits.
limitResources :: Bool -> IO ()
limitResources rlimit = do nice 20 -- Set our process priority way down
                           when rlimit $ mapM_ (uncurry setResourceLimit) limits

-- | Set all the available rlimits.
--   These values have been determined through trial-and-error
stackSizeLimitSoft, stackSizeLimitHard, totalMemoryLimitSoft, totalMemoryLimitHard,
 dataSizeLimitSoft, openFilesLimitSoft, openFilesLimitHard, fileSizeLimitSoft, fileSizeLimitHard,
 dataSizeLimitHard, cpuTimeLimitSoft, cpuTimeLimitHard, coreSizeLimitSoft, coreSizeLimitHard, zero :: ResourceLimit
totalMemoryLimitSoft = dataSizeLimitSoft
totalMemoryLimitHard = dataSizeLimitHard
-- These limits seem to be useless?
stackSizeLimitSoft = zero
stackSizeLimitHard = zero
-- We allow a few files to be opened, such as package.conf, because they are necessary. This
-- doesn't seem to be security problem because it'll be opened at the module
-- stage, before code ever evaluates. I hope.
openFilesLimitSoft = openFilesLimitHard
openFilesLimitHard = ResourceLimit 7
-- TODO: It would be nice to set these to zero, but right now Hint gets around the
-- insecurity of the GHC API by writing stuff out to a file in /tmp, so we need
-- to allow our compiled binary to do file I/O... :( But at least we can still limit
-- how much we write out!
fileSizeLimitSoft = fileSizeLimitHard
fileSizeLimitHard = ResourceLimit 10800
dataSizeLimitSoft = dataSizeLimitHard
dataSizeLimitHard = ResourceLimit $ 6^(12::Int)
-- These should not be identical, to give the XCPU handler time to trigger
cpuTimeLimitSoft = ResourceLimit 4
cpuTimeLimitHard = ResourceLimit 5
coreSizeLimitSoft = coreSizeLimitHard
coreSizeLimitHard = zero

-- convenience
zero = ResourceLimit 0

limits :: [(Resource, ResourceLimits)]
limits = [ (ResourceStackSize,    ResourceLimits stackSizeLimitSoft stackSizeLimitHard)
         , (ResourceTotalMemory,  ResourceLimits totalMemoryLimitSoft totalMemoryLimitHard)
         , (ResourceOpenFiles,    ResourceLimits openFilesLimitSoft openFilesLimitHard)
         , (ResourceFileSize,     ResourceLimits fileSizeLimitSoft fileSizeLimitHard)
         , (ResourceDataSize,     ResourceLimits dataSizeLimitSoft dataSizeLimitHard)
         , (ResourceCoreFileSize, ResourceLimits coreSizeLimitSoft coreSizeLimitHard)
         , (ResourceCPUTime,      ResourceLimits cpuTimeLimitSoft cpuTimeLimitHard)]

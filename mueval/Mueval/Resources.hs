module Mueval.Resources (limitResources) where

import Control.Monad (zipWithM_)
import System.Posix.Process (nice)
import System.Posix.Resource -- (Resource(..), ResourceLimits, setResourceLimit)
import System.Directory (setCurrentDirectory)

-- | Pull together several methods of reducing priority and easy access to resources:
--   nice, rlimits, and "cd".
limitResources :: IO ()
limitResources = do setCurrentDirectory "/tmp" -- will at least mess up relative links
                    nice 19 -- Set our process priority way down
                    zipWithM_ (setResourceLimit) resources limits

-- | Set all the available rlimits.
--   These values have been determined through trial-and-error
totalMemoryLimitSoft, totalMemoryLimitHard, stackSizeLimitSoft, stackSizeLimitHard,
 openFilesLimitSoft, openFilesLimitHard, fileSizeLimitSoft, fileSizeLimitHard, dataSizeLimitSoft,
 dataSizeLimitHard, cpuTimeLimitSoft, cpuTimeLimitHard, coreSizeLimitSoft, coreSizeLimitHard, zero :: ResourceLimit
totalMemoryLimitSoft = dataSizeLimitSoft
totalMemoryLimitHard = dataSizeLimitHard
-- These limits seem to be useless
stackSizeLimitSoft = zero
stackSizeLimitHard = zero
-- We allow one file to be opened, package.conf, because it is necessary. This
-- doesn't seem to be security problem because it'll be opened at the module
-- stage, before code ever evaluates.
openFilesLimitSoft = openFilesLimitHard
openFilesLimitHard = ResourceLimit 7
fileSizeLimitSoft = fileSizeLimitHard
fileSizeLimitHard = zero
dataSizeLimitSoft = dataSizeLimitHard
dataSizeLimitHard = ResourceLimit $ 5^(12::Int)
-- These should not be identical, to give the XCPU handler time to trigger
cpuTimeLimitSoft = ResourceLimit 3
cpuTimeLimitHard = ResourceLimit 4
coreSizeLimitSoft = coreSizeLimitHard
coreSizeLimitHard = zero
zero = ResourceLimit 0

resources :: [Resource]
resources = [ResourceStackSize,
             ResourceTotalMemory,
             ResourceOpenFiles,
             ResourceFileSize,
             ResourceDataSize,
             ResourceCoreFileSize,
             ResourceCPUTime]
limits :: [ResourceLimits]
limits = [  (ResourceLimits stackSizeLimitSoft stackSizeLimitHard)
          , (ResourceLimits totalMemoryLimitSoft totalMemoryLimitHard)
          , (ResourceLimits openFilesLimitSoft openFilesLimitHard)
          , (ResourceLimits fileSizeLimitSoft fileSizeLimitHard)
          , (ResourceLimits dataSizeLimitSoft dataSizeLimitHard)
          , (ResourceLimits coreSizeLimitSoft coreSizeLimitHard)
          , (ResourceLimits cpuTimeLimitSoft cpuTimeLimitHard)]

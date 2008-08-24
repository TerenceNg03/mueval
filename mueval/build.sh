#!/bin/sh
# Abort if any commands aren't successful
set -e
# Build
(runhaskell Setup configure --user && runhaskell Setup build && runhaskell Setup haddock && runhaskell Setup install || exit) &&
# Run the test suite with various options
echo "\n...Single-threaded tests....\n" &&
sh tests.sh &&
echo "\n...Rerun the tests with multiple threads...\n" &&
sh tests.sh +RTS -N4 -RTS &&
echo "\n...Rerun tests with resource limits enabled...\n" &&
sh tests.sh --rlimits &&
echo "\n...Done, apparently everything worked!"

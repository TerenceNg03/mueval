#!/bin/sh
# tests

# Save typing
m () { mueval --inferred-type --expression "$@"; }

# Abort if any commands aren't successful
set -e
# Test on valid expressions. Note we conditionalize - all of these should return successfully.
echo "Test some valid expressions \n"
## Does anything work?
m '1*100+1'
## OK, let's try some simple math.
m '(1*100) +1+1' --module Control.Monad
## String processing
m "filter (\`notElem\` ['A'..'Z']) \"abcXsdzWEE\""
## see whether we gave it enough resources to do reasonably long stuff
m "(last \"nebbish\") : (head $ reverse \"fooo bar baz booooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooreally long strong, you must confess, at least relative to the usual string, I suppose\") : []"
m 'let return a k = k a ; m >>= f = m . flip f ; foldM f z [] = return z ; foldM f z (x:xs) = f z x >>= \fzx -> foldM f fzx xs ; control e k = e (\a -> const (k a)) id in foldM (\p n -> if n == 0 then control (const $ return 0) else return (p * n)) 1 [-10..] id'
## Test whether we can import multiple modules
m 'join [[1]]' --module Data.List --module Control.Monad --module Data.Char
m 'join ["baz"]' --module Data.List --module Data.Char --module Control.Monad
m 'map toUpper "foobar"' --module Data.List --module Data.Char --module Control.Monad
m 'tail $ take 50 $ repeat "foo"' --module Data.List --timelimit 3
## This tests whether the SimpleReflect stuff is working. Output should be: "(f 1 (f 2 (f 3 (f 4 (f 5 z)))))\"
m 'foldr (\x y -> concat ["(f ",x," ",y,")"]) "z" (map show [1..5])'
## Test 1024-char limit
m 'repeat 1'
## Let's see whether the ShowQ instances for QuickCheck work
m 'myquickcheck (1+1 == 2)' -E
m 'myquickcheck (\x -> x == x)' -E
echo "\nOK, all the valid expressions worked out well." &&

# Test on bad or outright evil expressions
echo "Now let's test various misbehaved expressions \n" &&
## test infinite loop
m 'let x = x in x' ||
m 'let x y = x 1 in x 1' --timelimit 3 ||
m 'let x = x + 1 in x' ||
## Similarly, but with a strict twist
m 'let f :: Int -> Int; f x = f $! (x+1) in f 0' ||
## test stack limits
m 'let x = 1 + x in x' ||
## Let's stress the time limits
m 'let {p x y f = f x y; f x = p x x} in f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f f)))))))))))))))))) f' ||
## Now let's test the module whitelisting
m 1+1 --module Data.List --module System.IO.Unsafe --module Control.Monad ||
m "let foo = unsafePerformIO readFile \"/etc/passwd\" in foo" --module System.IO.Unsafe ||
m "head [1..]" --module Data.List --module Text.HTML.Download ||
### Can we bypass the whitelisting by fully qualified module names?
m "Foreign.unsafePerformIO $ readFile \"/etc/passwd\"" ||
m "Data.ByteString.Internal.inlinePerformIO $ readFile \"/etc/passwd\"" ||
## We need a bunch of IO tests, but I guess this will do for now.
m "let foo = readFile \"/etc/passwd\" >>= print in foo" ||
m "writeFile \"tmp.txt\" \"foo bar\"" ||
## Evil array code, should fail (but not with a segfault!)
m  "array (0::Int, maxBound) [(1000000,'x')]" --module Data.Array ||
echo "Done, apparently successfully"
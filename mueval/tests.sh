#!/bin/sh
# tests

# Save typing
alias mu='mueval "$@"'

# Test on valid expressions. Note we conditionalize - all of these should return successfully.
echo "Test some valid expressions\n"
## Does anything work?
mu --expression '1*100+1' &&
## OK, let's try some simple math.
mu --module Control.Monad --expression '(1*100) +1+1' &&
## String processing
mu --expression "filter (\`notElem\` ['A'..'Z']) \"abcXsdzWEE\"" &&
## see whether we gave it enough resources to do reasonably long stuff
mu --expression "(last \"nebbish\") : (head $ reverse \"fooo bar baz booooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooreally long strong, you must confess, at least relative to the usual string, I suppose\") : []" &&
mu --expression 'let return a k = k a ; m >>= f = m . flip f ; foldM f z [] = return z ; foldM f z (x:xs) = f z x >>= \fzx -> foldM f fzx xs ; control e k = e (\a -> const (k a)) id in foldM (\p n -> if n == 0 then control (const $ return 0) else return (p * n)) 1 [-10..] id' &&
## Test whether we can import multiple modules
mu --module Data.List --module Control.Monad --module Data.Char --expression 'join [[1]]' &&
mu --module Data.List --module Data.Char --module Control.Monad --expression 'join ["baz"]' &&
mu --module Data.List --module Data.Char --module Control.Monad --expression 'map toUpper "foobar"' &&
mu --module Data.List --timelimit 3 --expression 'tail $ take 50 $ repeat "foo"' &&
## This tests whether the SimpleReflect stuff is working. Output should be: "(f 1 (f 2 (f 3 (f 4 (f 5 z)))))\"
mu --expression 'foldr (\x y -> concat ["(f ",x," ",y,")"]) "z" (map show [1..5])' &&

# Test on bad/evil expressions
echo "\nNow let's test various misbehaved expressions\n"
## test infinite loop
mu --expression 'let x = x in x' ||
mu --timelimit 3 --expression 'let x y = x 1 in x 1' ||
mu --expression 'let x = x + 1 in x' ||
## Similarly, but with a strict twist
mu --expression 'let f :: Int -> Int; f x = f $! (x+1) in f 0' ||
## test stack limits
mu --expression 'let x = 1 + x in x' ||
## Let's stress the time limits
mu --expression 'let {p x y f = f x y; f x = p x x} in f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f f)))))))))))))))))) f' ||
## Now let's test the module whitelisting
mu --module Data.List --module System.IO.Unsafe --module Control.Monad --expression 1+1 ||
mu --module System.IO.Unsafe --expression "let foo = unsafePerformIO readFile \"/etc/passwd\" in foo" ||
mu --module Data.List --module Text.HTML.Download --expression "head [1..]" ||
### Can we bypass the whitelisting by fully qualified module names?
mu --expression "Foreign.unsafePerformIO $ readFile \"/etc/passwd\"" ||
mu --expression "Data.ByteString.Internal.inlinePerformIO $ readFile \"/etc/passwd\"" ||
## We need a bunch of IO tests, but I guess this will do for now.
mu --expression "let foo = readFile \"/etc/passwd\" >>= print in foo" ||
mu --expression "writeFile \"tmp.txt\" \"foo bar\"" ||
## Evil array code, should fail (but not with a segfault!)
mu --module Data.Array --expression "array (0::Int, maxBound) [(1000000,'x')]"

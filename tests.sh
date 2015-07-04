#!/bin/sh
# tests

# Save typing
m () { echo "$@" && mueval --inferred-type --expression "$@"; }

# Test whether it's around
mueval &> /dev/null

# Abort if any commands aren't successful
set -e
# Test on valid expressions. Note we conditionalize - all of these should return successfully.
echo "Test some valid expressions \n"
## Does anything work?
m 'True'
## Test comments
m 'True -- testing'
m 'True {- Testing -}'
## OK, let's try some simple math.
m '1*100+1'
m '(1*100) +1+1' --module Control.Monad
## String processing
m "filter (\`notElem\` ['A'..'Z']) \"abcXsdzWEE\""
## see whether we gave it enough resources to do reasonably long stuff
m "(last \"nebbish\") : (head $ reverse \"fooo bar baz booooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooreally long strong, you must confess, at least relative to the usual string, I suppose\") : []"
m 'let return a k = k a ; m >>= f = m . flip f ; foldM f z [] = return z ; foldM f z (x:xs) = f z x >>= \fzx -> foldM f fzx xs ; control e k = e (\a -> const (k a)) id in foldM (\p n -> if n == 0 then control (const $ return 0) else return (p * n)) 1 [-10..] id'
# Silly "Hello World"
m 'let uncat3 [] = [] ; uncat3 xs = (let (ys, zs) = splitAt 3 xs in ys : uncat3 zs) ; getFrom x y = map (x !!) $ map (fromIntegral . ((\x -> fromIntegral $ foldl (.|.) (0::Word8) (zipWith (\c n -> if c then bit n else (0::Word8)) x [0..2])) :: [Bool] -> Int)) $ reverse . uncat3 . reverse . concat . map (((\x -> map (testBit x) [7,6..0]) :: Word8 -> [Bool]) . fromIntegral . ord) $ y in getFrom " HWdelor" "e\184-\235"'
## Single module
m '()' --module=Prelude
## Test whether we can import multiple modules
m 'join [[1]]' --module Data.List --module Control.Monad --module Data.Char
m 'join ["baz"]' --module Data.List --module Data.Char --module Control.Monad
m 'map toUpper "foobar"' --module Data.List --module Data.Char --module Control.Monad
m 'tail $ take 50 $ repeat "foo"' --module Data.List --time-limit 3
## This tests whether the SimpleReflect stuff is working. Output should be: "(f 1 (f 2 (f 3 (f 4 (f 5 z)))))\"
m 'foldr (\x y -> concat ["(f ",x," ",y,")"]) "z" (map show [1..5])'
## Test 1024-char limit
m 'repeat 1'
## Test Unicode. If this fails, characters got mangled somewhere.
m 'let (ñ) = (+) in ñ 5 5'
## Test default imports & have some function fun
m 'fix (1:)'
m 'fix show'
m 'let fix f = let x = f x in x in fix show'
m '(+1) . (*2) $ 10'
m 'fmap fix return 42'
m 'filterM (const [False,True]) [1,2,3]'
m 'sequence [[1,2,3],[4,5]]'
m 'sort [4,6,1,2,3]'
m 'runIdentity $ mfix (return . (0:) . scanl (+) 1)'
m 'fix ((1:).(1:).(zipWith (+) `ap` tail))'
m "listArray (1,10) ['a'..]"
### Test Control.Arrow
m 'let f = (id *** id) in f (3, 4)'
### Test Control.Applicative
m "(foldr (liftA2 (||)) (const False) [isDigit, isAlpha]) '3'"
### Test SimpleReflect <http://twanvl.nl/blog/haskell/simple-reflection-of-expressions>
m "sum $ map (*x) [1..5]"
m "iterate (^2) x"
m "scanl f x [a,b,c]"
m "zipWith3 f [1,2..] [1,3..] [1,4..] :: [Expr]"
m "sum [1..5] :: Expr"
m "foldr f x [1..5]"
## Test defaulting of expressions
m '(+1) <$> [1..3]'
## Now let's do file loading
echo "module TmpModule (foo, bar) where { foo x = x + 1; bar x = x + 2 }" > "TmpModule.hs"
m '1+1' --load-file="TmpModule.hs"
m 'foo 1' --load-file="TmpModule.hs"
m "foo 1" -S --load-file="TmpModule.hs"
m 'bar 1' --load-file="TmpModule.hs"
m 'foo $ foo 1' --load-file="TmpModule.hs"
rm "TmpModule.hs"
## Test the --no-imports function
## TODO: more extensive tests of this
m '()' --no-imports
## Test naming individual syntactic extensions
m "let f (id -> x) = x in f 1" -XViewPatterns
m "let f :: Int -> State Int (); f (id -> x) = put x in runState (f 1) 1" --module Control.Monad.State -XViewPatterns -XFlexibleContexts
## Test Safe-Haskell-approved code
m "()" -S
m "runReader ask 42" -S --module Control.Monad.Reader
## Setup for later Safe-Haskell tests and ensure that behavior is as
## expected without SH activated
echo 'module TmpModule (unsafePerformIO) where {import System.IO.Unsafe}' > "TmpModule.hs"
m  'unsafePerformIO (readFile "/etc/passwd")' --load-file="TmpModule.hs"
## Test qualified imports
m "M.map (+1) $ M.fromList [(1,2), (3,4)]" &&
echo "\nOK, all the valid expressions worked out well." &&

# Test on bad or outright evil expressions
echo "Now let's test various misbehaved expressions \n" &&
## test infinite loop
m 'let x = x in x' ||
m 'let x y = x 1 in x 1' --time-limit 3 ||
m 'let x = x + 1 in x' ||
## Similarly, but with a strict twist
m 'let f :: Int -> Int; f x = f $! (x+1) in f 0' ||
## test stack overflow limits
m 'let x = 1 + x in x' ||
m 'let fix f = let x = f x in x in foldr (.) id (repeat read) $ fix show' ||
## Let's stress the time limits
m 'let {p x y f = f x y; f x = p x x} in f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f f)))))))))))))))))) f' ||
# Are evil functions in scope?
m 'runST (unsafeIOToST (readFile "/etc/passwd"))' ||
m 'unsafeCoerce (readFile "/etc/passwd"))' ||
### Can we bypass the whitelisting by fully qualified module names?
m 'Unsafe.unsafeCoerce (readFile "/etc/passwd"))' ||
m 'Foreign.unsafePerformIO $ readFile "/etc/passwd"' ||
m 'Data.ByteString.Internal.inlinePerformIO (readFile "/etc/passwd")' ||
## We need a bunch of IO tests, but I guess this will do for now.
m 'let foo = readFile "/etc/passwd" >>= print in foo' ||
m 'writeFile "tmp.txt" "foo bar"' ||
## Evil array code, should fail (but not with a segfault!)
m  "array (0::Int, maxBound) [(1000000,'x')]" --module Data.Array ||
## code that should be accepted without Safe Haskell but rejected with
m  'unsafePerformIO (readFile "/etc/passwd")' -S --load-file="TmpModule.hs" ||
echo "Done, apparently all evil expressions failed to do evil"

rm TmpModule.hs

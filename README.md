# What

Mueval grew out of my discontent with Lambdabot: it's really neat to be able to run expressions in `#haskell` like this:

    07:53 < ivanm> > filter (\ x -> isLetter x || x == '\t') "asdf$#$     dfs"
    07:55 < lambdabot>  "asdfdfs"

But Lambdabot is crufty and very difficult to install or run. IMO, we need a replacement or rewrite, but one of the things that make this difficult is that Lambdabot uses `hs-plugins` to get that sort of evaluation functionality, and `hs-plugins` is half the problem. We want some sort of standalone executable which provides that functionality. Now, `ghc -e` is obviously unsuited because there is no sandboxing, so what I've done is basically marry the GHC API (as rendered less sharp-edged by Hint) with a bunch of resource limits and sandboxing (as largely stolen from Lambdabot).

# Examples

The end result is an adorable little program, which you can use like this:

    $ mueval --expression '1*100+1'
    Expression type: (Num t) => t
    result: "101"

    $ mueval --expression "filter (\`notElem\` ['A'..'Z']) \"abcXsdzWEE\""
    Expression type: [Char]
    result: "\"abcsdz\""

Note that mueval will avoid all the attacks I've been able to test on it:

    $ mueval --expression 'let x = x in x'
    Expression type: t
    result: "mueval: Time limit exceeded"

    $ mueval --expression "let foo = readFile \"/etc/passwd\" >>= print in foo"
    Expression type: IO ()
    result: "<IO ()>"

    $ mueval --module System.IO.Unsafe --expression "let foo = unsafePerformIO readFile \"/etc/passwd\" in foo"
    mueval: Unknown or untrusted module supplied! Aborting.

## Loading definitions from files

Like Lambdabot, Mueval is capable of loading a file and its definitions. This is useful to get a kind of persistence. Suppose you have a file `L.hs`, with a function `bar = (+1)` in it; then `mueval --loadfile=L.hs --expression="bar 1"` will evaluate to, as one would expect, `2`.

It's worth noting that definitions and module imports in the loaded *are not* fully checked like the expression is. The resource limits and timeouts still apply, but little else. So if you are dynamically adding functions and module imports, you *must* secure them yourself or accept the loss of security. Currently, all known 'evil' expressions cause Mueval to exit with an error (a non-zero exit code), so my advice is to do something like `mueval --expression foo && echo "\n" >> L.hs && echo foo >> L.hs`. (That is, only accept new expressions which evaluate successfully.)

# Summary

Anyway, it's my hope that this will be useful as an example or useful in itself for people endeavoring to fix the Lambdabot situation or just in safely running code period.

# Getting

You can download Mueval at Hackage: <http://hackage.haskell.org/package/mueval>. Mueval has a public Git repository, at <https://github.com/gwern/mueval>. Contributions & updates are of course welcomed.

# Installing

Mueval depends on a few of the standard libraries, which you should have installed already, and also on the 'Hint' library <http://hackage.haskell.org/package/hint>; Hint is particularly essential as it is the very capable wrapper around the GHC API which Mueval uses. (Without Hint, this would've been much more painful to write). All of this is cabalized, so ideally installation will be as simple as:

    $ cabal install mueval

However, you can still manually download and unpack the Mueval tarball, and do the usual Cabal dance:

    $ runhaskell Setup configure
    $ runhaskell Setup build
    $ runhaskell Setup install

# See also

- Chris Done's interactive Haskell REPL website, [Try Haskell!](http://tryhaskell.org/)

# Bugs

Mueval uses a number of techniques for security; particularly problematic seem to be the resource limits, as they have to be specified manually & statically in the source code and so will probably be broken somewhere somewhen. For this reason, they are not enabled by default. Experiment with --rlimits for hours of fun!

Mueval also cannot do qualified imports. This is due to limitations in the GHC API; see <https://ghc.haskell.org/trac/ghc/ticket/1895> & <https://ghc.haskell.org/trac/ghc/ticket/2362>.

As of 2010 or so, compiling Mueval (or any Hint-using executable) with profiling support seems to lead to runtime crashes.

Finally, under GHC 6.10.1 (and higher?), you must run Mueval with `+RTS -N2 -RTS` as otherwise the watchdog threads will not get run and DoS attacks are possible. (Compare `mueval -e "let x = x + 1 in x"` against `mueval -e "let x = x + 1 in x" +RTS -N2 -RTS`.)

# Contributions

So, you've discovered a bug or other infelicity? If you can successfully build & install Mueval, but running it on expressions leads to errors, please send me an email at <gwern@gwern.net>. Include in the email all the output you see if you run the informal test suite:

    $ sh tests.sh

If this script *does not* terminate with a success message, then there's probably something wrong. One of the properties Mueval strives to have is that on every bad expression, it errors out with an exit code of 1, and on every good expression, an exit code of 0.

Also good is making sure `cabal check` and `hlint` are happy; but that's not as important as `tests.sh` passing.

# License

BSD-3.

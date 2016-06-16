# An impure lazy programming language

## Introduction

I will explain how, if you start with an impure lazy programming
language, you very quickly discover that you may as well make it pure
and handle side effects through an `IO` type.

## Unclean

I've got a strongly-typed, impure, lazy, programming language called
"Unclean".  Impure means that function application can have side
effects.  Lazy means that function arguments are not evaluated until
they are demanded and the results of computations bound to an
identifier are memoized.

## Laziness and memoization

One part of laziness is that function arguments are not evaluated
until and unless they are used.  In the following example the value of
`x` is never demanded, so it is never calculated and the expression is
quickly evaluated to `0`.

````haskell
> let x = sum [1..10000000] in "Hello"
"Hello"
````

The other part of laziness is that the result of an expression bound
to an identifier is memoized (cached) for reuse.  In the following
example the first time we evaluate `y` the computation bound to it is
run and there is some delay before we see the result.  The second time
we evaluate `y` the result is returned instantly because it has
already been memoized.

````haskell
> let y = sum [1..10000000]
> y
50000005000000
-- That was slow
> y
50000005000000
-- That was quick
````

## Using Unclean

Here are a couple of entries in the Unclean standard library:

````haskell
putStrLn :: String -> ()

getLine :: String
````

And here are examples of using them:

````
> putStrLn "Hello world"
Hello world
()
````

`putStrLn` prints its argument to the console and returns `()`.  I can
also print the result of reading a line of input:

````
> putStrLn getLine
I entered this text
I entered this text
()
````

There's also a function `seq :: a -> b -> b` which evaluates its first
argument and then returns its second.  I can use `seq` to write
sequences of code that interact with the user.

````haskell
main2 :: ()
main2 = putStrLn "What is your name?"
        `seq`
        putStrLn ("Hello " ++ getLine)
````

````
> main2
What is your name?
Hello Tom
Tom
()
````

Hmm, it doesn't quite do what I want though.  Due to laziness it
started printing "Hello" before it asked for my name.  How about the
following instead?

````haskell
main3 :: ()
main3 = let g = getLine
        in putStrLn "What is your name?"
           `seq`
           g
           `seq`
           putStrLn ("Hello " ++ g)
````

````
> main3
What is your name?
Tom
Hello Tom
()
````

That's much better.  But what if I want to read two strings with
`getLine`?  I can try

````haskell
main4 :: ()
main4 = let name     = getLine
            language = getLine
        in putStrLn "What is your name?"
           `seq`
           name
           `seq`
           putStrLn "What is your favourite programming language?"
           `seq`
           language
           `seq`
           putStrLn (name ++ " likes " ++ language)
````

````
> main4
What is your name?
Tom
What is your favourite programming language?
Tom likes Tom
()
````

That's strange.  It only asked for my name and the told me that I like
myself.  Whilst technically true this is not what I wanted.  What's
going on?  Unclean memoizes `getLine` and and so we see the same
result in all its uses.  That's no good at all!  It will mean that we
can only ever read one line in an Unclean program.  To avoid
memoization we can make `getLine` a function.

````haskell
getLineF :: () -> String
````

We apply a `()` argument to `getLineF` each time it is used and thus
we don't memoize.

````
main5 :: ()
main5 = putStrLn "What is your name?"
        `seq`
        let name = getLineF ()
        in name
        `seq`
        putStrLn "What is your favourite programming language?"
        `seq`
        let language = getLineF ()
        in language
        `seq`
        putStrLn (name ++ " likes " ++ language)
````

````
> main5
What is your name?
Tom
What is your favourite programming language?
Unclean
Tom likes Unclean
()
````

That's the result we wanted.  It's a bit annoying that we have to
mention our variables twice when we bind them.  `let name = getLineF
() in name ...` is awkward and sure to lead to programming mistakes.
How far can we abstract that away?  The first thing to try is this
`andThen` function.

````haskell
andThen :: b -> (b -> c) -> c
andThen x f = x `seq` f x
````

It allows us to write

````haskell
main6 :: ()
main6 = putStrLn "What is your name?"
                    `andThen` \_ ->
        getLineF () `andThen` \name ->
        putStrLn "What is your favourite programming language?"
                    `andThen` \_ ->
        getLineF () `andThen` \language ->
        putStrLn (name ++ " likes " ++ language)
````

which runs just like `main5`.  `andThen` works to essentially bind the
result of a function to a name.  It's still syntactically awkward
though.  Let's invent a "`do` notation" to make it syntactically more
convenient.

````haskell
main7 :: ()
main7 = do
  putStrLn "What is your name?"
  name <- getLineF ()
  putStrLn "What is your favourite programming language?"
  language <- getLineF ()
  putStrLn (name ++ " likes " ++ language)

  where (>>=)     = andThen
        (>>) x y  = x >>= (\_ -> y)
        return    = id
        fail      = error
````

`main7` also runs just like `main5`.  This `do` notation has nothing
to do with monads.  It's just a syntactic transformation that makes
working with `andThen` more convenient.

Unfortunately it still doesn't stop us doing weird things like this:

````haskell
main8 :: ()
main8 = do
  let g = getLineF ()
  putStrLn "What is your name?"
  name <- g
  putStrLn "What is your favourite programming language?"
  language <- g
  putStrLn (name ++ " likes " ++ language)

  where (>>=)     = andThen
        (>>) x y  = x >>= (\_ -> y)
        return    = id
        fail      = error
````

````
> main8
What is your name?
Tom
What is your favourite programming language?
Tom likes Tom
()
````

There's still too much memoization.  Ideally we don't want to memoize
the results of side effects at all.  Let's wrap up our side-effecting
actions in a datatype which always prevents memoization.

````haskell
data IO a = IO (() -> a)

unIO :: IO a -> a
unIO (IO a) = a ()

andThenIO :: IO a -> (a -> IO b) -> IO b
andThenIO x f = IO (\_ -> unIO x `andThen` (unIO . f))

putStrLnIO :: String -> IO ()
putStrLnIO s = IO (\_ -> putStrLn s)

getLineIO :: IO String
getLineIO = IO getLineF
````

Then nothing goes wrong if we try to get a line in two places

````haskell
main9 :: ()
main9 = unIO $ do
  let g = getLineIO

  putStrLnIO "What is your name?"
  name <- g
  putStrLnIO "What is your favourite programming language?"
  language <- g
  putStrLnIO (name ++ " likes " ++ language)

  where (>>=)     = andThenIO
        (>>) x y  = x >>= (\_ -> y)
        return x  = IO (\_ -> x)
        fail      = error
````

`main9` also runs just like `main5`.  This setup is still not quite
right though.  It doesn't prevent us from writing

````haskell
main10 :: ()
main10 = unIO $ do
  let g = getLine

  putStrLnIO "What is your name?"
  let name = g
  putStrLnIO "What is your favourite programming language?"
  let language = g
  putStrLnIO (name ++ " likes " ++ language)

  where (>>=)     = andThenIO
        (>>) x y  = x >>= (\_ -> y)
        return x  = IO (\_ -> x)
        fail      = error
````

which has the same problem as `main8`.  We are now, however, in a
position to solve all these problems once and for all.

## Purity

The solution is to wrap *all* side-effecting primitives in `IO`, and
ensure that `IO` is an abstract datatype whose contents we can't
unwrap.  This way we can only combine side-effecting actions by using
`andThenIO`, never directly.  This ensures that side effects always
occur in the order we expect and function application itself never has
side effects.  At this point Unclean has become a *pure* functional
programming language and it is, in fact, Haskell.

## Conclusion

If you have an impure, lazy, functional programming language the only
natural way to handle side effects is to ensure that they are all
wrapped in a datatype (called `IO` in Haskell, but it could be called
anything you like) and provide combinators for ordering them relative
to each other.

At this point the notion of "monad" is floating around in the
background, but that notion neither is needed to *discover* how to
handle side effects sanely in a lazy language nor is needed to
understand how to *use* the `IO` type in practice.  If the inventors
of Haskell had started with an impure lazy language (with `seq`) and
asked themselves "how do we sanely handle the ordering of effects in
this language?"  they would probably have quickly discovered this very
natural approach.

(Thanks to Sam Davis for a review)

## Appendix: Getting Unclean

You too can have your very own Unclean compiler.  Just add some lines
like this at the top of your source files:

````haskell
{-# LANGUAGE RebindableSyntax #-}

import System.IO.Unsafe
import qualified Prelude

u = unsafePerformIO

putStrLn :: String -> ()
putStrLn = u . Prelude.putStrLn

getLine :: String
getLine = u Prelude.getLine

getLineF :: () -> String
getLineF _ = u Prelude.getLine
````

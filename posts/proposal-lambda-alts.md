# Proposal: `LambdaAlts`

NOTE: When this is published link it at
https://twitter.com/purelyagile/status/1349333019874758662

## `LambdaCase`

`LambdaCase` is a wonderful GHC language extension.  62% of
respondents to the [2020 Haskell
survey](https://taylor.fausak.me/2020/11/22/haskell-survey-results/#s2q5)
said that they would like it to be enabled by default.  Why is it so
popular?  Personally I really like the syntactic decluttering role
that it plays. I enable it in roughly half of the modules that I
write.  Specifically I find

```haskell
myFunction = \case
  Foo a b -> ...
  Bar x y -> ...
```

much neater than

```haskell
myFunction x = case x of
  Foo a b -> ...
  Bar x y -> ...
```

The name `x` is completely redundant!  I don't want to mention it.
Another common alternative is

```haskell
myFunction (Foo a b) = ...
myFunction (Bar x y) = ...
```

but I find myself using this form only when combining it with pattern
matching on other arguments.  Personally I like [Jón Fairbairn's
suggested
syntax](https://mail.haskell.org/pipermail/haskell-cafe/2012-November/104884.html)
better, but that ship has sailed and `LambdaCase` as it is is jolly
good.

Basically `LambdaCase` gives us a form of "generalised eta reduction"
for `case` expressions.

```haskell
myFunction a = f (g a)
```

and

```haskell
myFunction a = \case
  G g -> ...
  H h -> ...
```

avoid the redundant names that we would be required to conjure up if
we wrote

```haskell
myFunction a b = f (g a) b
```

and

```haskell
myFunction a b = case b of
  G g -> ...
  H h -> ...
```

## `LambdaAlts`

If you find the ability to "generalised eta reduce" beneficial then
here's another proposal that you might like.  I don't want to write

```haskell
\case
   Just x  -> foo x
   Nothing -> ...
```

The `x` is redundant!  Nor do I want to write

```haskell
\case
   Left l  -> handle l
   Right r -> continue r
```

Instead I just want to write

```haskell
\case
   Just    -> foo
   Nothing -> ...
```

and

```haskell
\case
   Left  -> handle
   Right -> continue
```

In general I want to be able to "eta reduce" a case alternative

```haskell
   Constructor fn ... f2 f1 -> function fm ... f2 f1
```

to

```haskell
   Constructor fn fm+1 -> function
```

## Destructor functions

We already have something *similar* to this for some built-in sum
types.  There's the function `either :: (a -> r) -> (b -> r) -> Either
a b -> r` and `maybe :: a -> (b -> a) -> Maybe b -> a`.  Hang on,
should that be `maybe :: (b -> a) -> a -> Maybe b -> a`.  I genuinely
can't remember, though I think I got it right the first time.

Wouldn't it be better if I didn't have to remember the argument
order?  Instead of writing

```haskell
maybe theDefault processTheJust myMaybe
```

I would much rather take advantage of `LambdaCase` and write

```haskell
(\case { Nothing -> theDefault; Just x -> processTheJust x }) myMaybe
```

It's a little bit longer but much more explicit.  The other benefit is
the `LambdaCase` version comes for free.  If I define a new sum type

```haskell
data T = A Foo1 Foo2 | B Bar | C Baz
```

then I don't have to arbitrarily choose a constructor order and define

```haskell
t :: (Foo1 -> Foo2 -> r) -> (Bar -> r) -> (Baz -> r) -> T -> r
```

I can just use `\case` like

```haskell
\case { A foo1 foo2 -> ...; B bar -> ...; C baz -> ... }
```

`LambdaAlts` makes this form even neater.  I can write

```haskell
\case { A -> ...; B -> ...; C -> ... }
```

if the expressions on the right hand sides were functions applied
directly to the constructor field.


## Real world example

```
tcUnderlyingFunTyped fun ty = case baseFunArgTy fun ty of
  Right baseTy -> pure (toUserFunTyped @p fun baseTy)
  Left err -> addErr err
```

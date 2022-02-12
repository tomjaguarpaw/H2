# Tail recursion modulo cons combinators

## Recursion combinators

In Haskell we can write a function that adds `1` to every element of a
list as follows

```haskell
addOne :: [Int] -> [Int]
addOne = \case
  [] -> []
  x:xs -> (x + 1) : addOne xs
```

and we can write a function that sums a list as follows

```haskell
sum :: Int -> [Int] -> Int
sum !total = \case
  [] -> total
  x:xs -> sum (x + total) xs
```

Typically though, we'd prefer to use recursion combinators designed
for the job:

```haskell
addOne = map (+1)
sum = foldl' (+)

-- where
map :: (a -> b) -> [a] -> [b]
map f = \case
  [] -> []
  x:xs -> f x : map f xs

foldl' :: (s -> a -> s) -> s -> [a] -> s
foldl' f !z = \case
  [] -> z
  x:xs -> foldl' f (f z x) xs
```

Why?  One reason is that a recursion combinator describe the "shape"
or "structure" of a recursion so that we can easily know properties
about a function implemented using it without careful analysis of the
body of the function.  For example, the output of the function written
as

```haskell
addOne = \case
  [] -> []
  x:xs -> (x + 1) : addOne xs
```

has the same length as its input, but you have to carefully analyse the
body of the function to work that out.  Writing `addOne = map (+1)`
instead makes that property obvious.  That property is simply a
property that `map` has!  Once you know it, you know it every time you
see it.  Similarly, during evaluation of

```
sum !total = \case
  [] -> total
  x:xs -> sum (x + total) xs
```

the input list is consumed linearly and incrementally, that is, the
elements of the list are read from left to right and once they have
been read they are discarded.  Again, to convince yourself of that you
have to carefully read the implementation.  Writing `sum = foldl' (+)`
makes it obvious.  That property is simply a property that `foldl'`
has!  Once you know it, you know it every time you see it.

## Tail recursion

Tail recursion is, informally speaking, a form of recursion where any
recursive call is "the last thing the function does".  In particular
no resources from the body are held on to once the recursive call is
invoked.  `sum` above is an example of a tail recursive function.
Writing it in the following form makes it clearer how the "tail" and
"last thing the function does" terminology applies.

```haskell
sum !total = \case
  [] -> total
  x:xs ->
    let nextTotal = x + total
    in sum nextTotal xs
```

Either the function finishes, returning `total`, or it proceeds with
calculation, finally making a tail recursive call.  Nested expression
can always be converted into a sequence of `let` expressions, and a
tail recursive function is one in which, when you do so, the recursive
call only ever appears as the body of a let, i.e. after the `in`.

`foldl'` is also tail recursive.  To see that we can write it as

```haskell
foldl' f !z = \case
  [] -> z
  x:xs ->
    let nextZ = f z x
    in foldl' f nextZ xs
```

Either it returns with its result `z`, or it calls itself tail
recursively, i.e. after the `in`.

## The tail recursion combinator

There is a combinator that abstracts the notion of tail recursion
itself!

```haskell
loop :: (s -> Either s r) -> s -> r
loop f s = case f s of
  Right r -> r
  Left nextS -> loop f nextS
```

We pass it a function `f` and an initial state `s`.  The combinator
runs `f` on the state.  If `f` returns a `Right r` then `r` is the
final result.  If `f` returns a `Left nextS` then `nextS` is the new
state for the tail recursion.

We can write `foldl'` in terms of `loop` by considering the pair of
accumulator `z` and list `xs` as the state.

```haskell
foldl' :: (s -> a -> s) -> s -> [a] -> s
foldl' f z xs = loop (\case
  -- Either we have run out of list, so we are done
  (z, []) -> Right z
  -- Or we have another list element, in which case
  -- we combine it with `z` and recurse on the tail
  (!z, x:xs) -> Left (f z x, xs))
  (z, xs)
```

## Tail recursion modulo bind

What about the following function?  Is it tail recursive?

```haskell
countdown :: Int -> IO ()
countdown n =
  if n <= 0
  then putStrLn "Lift off!"
  else do print n
          countdown (n - 1)

-- > countdown 5
-- 5
-- 4
-- 3
-- 2
-- 1
-- Lift off!
```

It looks like the recursive call to `countdown` is "the last thing
that happens" but if we rewrite `countdown` into the list of lets form
then we see that the recursive call *doesn't* happen immediately after
the `in`.


```haskell
countdown n =
  if n <= 0
  then putStrLn "Lift off!"
  else let m1 = print n
           m2 = countdown (n - 1)
       in m1 >> m2
```

Still, no resources from the body are held on to after the recursive
call is invoked

TODO:

```haskell
loopM :: Monad m => (s -> m (Either s r)) -> s -> m r
loopM f s = do
  f s >>= \case
    Left nextS -> loopM f nextS
    Right r -> pure r
```

## Tail recursion modulo cons

TODO:

```haskell
loopL :: (s -> Maybe (x, s)) -> s -> [x]
loopL f s = case f s of
  Nothing -> []
  Just (x, nextS) -> x : loopL f nextS
```

cons[^1]

[^1]: blah

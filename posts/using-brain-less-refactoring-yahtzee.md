# Using our brain less in refactoring Yahtzee

Cameron Gera and Taylor Fausak [produced a
podcast](https://haskellweekly.news/episode/22.html) on [an article of
mine about good design and
typesafety](../good-design-and-type-safety-in-yahtzee/).  The article is
about refactoring code to improve design and how that goes
hand-in-hand with type safety.  Intriguingly, listening to others talk
about my article gave me a new perspective.

At one point we observed that a variable to an argument was unused.
Eventually we removed it.  The only justification given for removing
it was to convince ourselves that it was unused by looking at the
implementation of the function and to insert a run time check.

Hearing Cameron and Taylor talk about the article made me think again.
There were only two changes to the code that really relied upon
understanding what it does; everything else was mechanical
transformation.  Both of those changes were to do with the unused
argument.

Einstein said "chalk is cheaper than grey matter".  Can we avoid
using "grey matter" (our brains) to remove the unused argument,
instead just relying on "chalk" (mechanical transformations)?  The
answer is yes!  Let's see how to do it.

### The starting point

We start from the "Add `pop` function" stage of the [previous
article](../good-design-and-type-safety-in-yahtzee/).  We've got a
suspicion that, although the argument `n` to `allRolls` *is* used, the
argument `n-1` to the recursive call is not.  How can we transform the
code to make that clear?

```haskell
type DiceChoice = [ Bool ]
type DiceVals   = [ Integer ]
type DiceState  = (DiceVals, Integer)

pop :: DiceChoice
    -> DiceVals
    -> Maybe ((Bool, Integer), (DiceChoice, DiceVals))
pop [] [] = Nothing
pop (chosen:choices) (v:vs) = Just ((chosen, v), (choices, vs))
pop (_:_) [] = error "Invariant violated: missing val"
pop [] (_:_) = error "Invariant violated: missing choice"

allRolls :: DiceChoice -> DiceState -> [ DiceState ]
allRolls choices (vs, n) = case pop choices vs of
  Nothing -> [ ([], n-1) ]
  Just ((chosen, v), (choices, vs)) ->
    allRolls choices (vs, n-1) >>=
        \(roll,_) -> [ (d:roll,  n-1) | d <- rollList ]
          where rollList = if chosen then [v] else [ 1..6 ]

example =
  let diceChoices = [ False, True, True, False, False ]
      diceVals = [ 6, 4, 4, 3, 1 ]
  in mapM_ print $ allRolls diceChoices (diceVals, 2)
```

### Use `do`-notation

Let's immediately simplify by using `do`-notation.  In the [previous
article](../good-design-and-type-safety-in-yahtzee/) we left this stage
until later but given that the recursive call is currently part of a
`>>=` expression let's apply the simplification now.

```haskell
allRolls :: DiceChoice -> DiceState -> [ DiceState ]
allRolls choices (vs, n) = case pop choices vs of
  Nothing -> [ ([], n-1) ]
  Just ((chosen, v), (choices, vs)) -> do
    (roll, _) <- allRolls choices (vs, n-1)
    [ (d:roll, n-1) | d <- rollList ]
          where rollList = if chosen then [v] else [ 1..6 ]
```

### Observe that both branches pair a list with `n-1`

If the argument `n` were not used at all then our job would be much
easier.  However, it *is* used, so let's try to separate the place it
is used from the place where (we believe) it is not used.

Where is it used?  We can see that each branch of the `case` statement
returns a list of tuples where the second element of each tuple is
`n-1`.  Put another way, each branch produces a list and then maps the
"pair with `n-1`" function over it.

I'll write the "pair with `n-1`" function as `(, n-1)` (using the
`TupleSections` extension).  The usual alternative would be to write
`\x -> (x, n-1)` but in this article I want to keep things compact.

```
 allRolls :: DiceChoice -> DiceState -> [ DiceState ]
 allRolls choices (vs, n) = case pop choices vs of
-  Nothing -> [ ([], n-1) ]
+  Nothing -> fmap (, n-1) [ [] ]
   Just ((chosen, v), (choices, vs)) -> do
     (roll, _) <- allRolls choices (vs, n-1)
-    [ (d:roll,  n-1) | d <- rollList ]
+    fmap (, n-1) [ d:roll | d <- rollList ]
```

### Lift `fmap` outside `do`

Now `n` is used, we think, just twice, and in each case mapping the
"pair with `n-1`" function over a list.  We've made this duplication
obvious but we can't yet remove it.  First we have to lift the `fmap`
outside the `do`.  We use the rule that

```haskell
do ...
   fmap f e
```

can be rewritten to

```haskell
fmap f $ do ...
            e
```

Why is this rewriting valid?  Informally, a `do` block is like a
procedure, and this rule says that "applying `f` and then returning
from the procedure" is the same as "returning from the procedure and
then applying `f`".  Formally, it can be proved using the monad laws.

This is how the rewriting applies in our case:

```
-  Just ((chosen, v), (choices, vs)) -> do
+  Just ((chosen, v), (choices, vs)) -> fmap (, n-1) $ do
     (roll, _) <- allRolls choices (vs, n-1)
-    fmap (, n-1) [ d:roll | d <- rollList ]
+    [ d:roll | d <- rollList ]
```


### Combine duplicated functions at top level

Now that both branches of the `case` statement are `fmap (, n-1)` of
something we can apply the `fmap (, n-1)` to the overall `case`
statement instead.  Specifically, the rule is that we can rewrite

```haskell
case x of
    Case1 -> f $ body1
    Case2 -> f $ body2
```

to

```haskell
f $ case x of
    Case1 -> body1
    Case2 -> body2
```

which in our code leads to

```
-allRolls choices (vs, n) = case pop choices vs of
-  Nothing -> fmap (, n-1) [ [] ]
-  Just ((chosen, v), (choices, vs)) -> fmap (, n-1) $ do
+allRolls choices (vs, n) = fmap (, n-1) $ case pop choices vs of
+  Nothing -> [ [] ]
+  Just ((chosen, v), (choices, vs)) -> do
```

### Split function body into separate function

We want to carefully separate the parts of the code for which the
value of `n` matters from the parts of the code for which the value of
`n` does not matter.  To this end we split the body of `allRolls` into
a separate function called `allRollsBody`.

```
 allRolls :: DiceChoice -> DiceState -> [ DiceState ]
-allRolls choices (vs, n) = fmap (, n-1) $ case pop choices vs of
+allRolls choices (vs, n) = fmap (, n-1) $ allRollsBody choices (vs, n)
+
+allRollsBody :: DiceChoice -> DiceState -> [ DiceVals ]
+allRollsBody choices (vs, n) = case pop choices vs of

```

### Substitute definition of `allRolls`

Now we are in the nice situation that, although we are yet to prove it
to our satisfaction, the value of `allRollsBody` does not depend on
its argument `n`.

However, we've ended up with a pair of mutually recursive functions!
That's somewhat unusual.  In order to make `allRollsBody` recurse only
on itself we substitute the definition of `allRolls` back into
`allRollsBody`.  Additionally, that makes `allRolls` not recursive at
all.

```
-    (roll, _) <- allRolls choices (vs, n-1)
+    (roll, _) <- fmap (, n-1) $ allRollsBody choices (vs, n-1)
```

### Remove redundant pairing

We're pairing every element of a list with `n-1` and then immediately
removing it.  Let's just avoid the pairing in the first place.

```
-    (roll, _) <- fmap (, n-1) $ allRollsBody choices (vs, n-1)
+    roll <- allRollsBody choices (vs, n-1)
```

### Generalise type of `allRollsBody`

Now the magic happens!  Our code currently looks like this.

```haskell
allRolls :: DiceChoice -> DiceState -> [ DiceState ]
allRolls choices (vs, n) = fmap (, n-1) $ allRollsBody choices (vs, n)

allRollsBody :: DiceChoice -> DiceState -> [ DiceVals ]
allRollsBody choices (vs, n) = case pop choices vs of
  Nothing -> [ [] ]
  Just ((chosen, v), (choices, vs)) -> do
    roll <- allRollsBody choices (vs, n-1)
    [ d:roll | d <- rollList ]
          where rollList = if chosen then [v] else [ 1..6 ]
```

Previously we had to use our brains to spot that the `Integer`
argument to the recursive call was unused.  We inserted a run time
check to convince ourselves that we were right.  Now we have split the
original function into two, only one of which contains a recursive
call.  We can see clearly that the only use for the argument `n` to
`allRollsBody` is to be modified and passed to the recursive call.
The value of that argument is never used in any other way.  From that
observation alone we are probably satisfied that we can remove it.

In fact we can go one step better.  We can make a small change to our
code so that we do not even have to inspect the implementation to know
that `n` is unused.  The compiler will check the property for us!
However, the check is demonstrated in a strange way, and if you're not
familiar with it then it will look utterly bizarre.

We generalise the type signature so that the function doesn't just
work for an `Integer` but rather for *any* type of numeric argument,
that is, any type with an instance of the `Num` type class.

```haskell
allRollsBody :: Num t => DiceChoice -> (DiceVals, t) -> [DiceVals]
```

Believe it or not, from this type signature alone, without knowing
anything about the implementation, we can conclude that the `t`
argument is not used!  How on earth can we conclude that?  It's
because of the
[parametricity](https://en.wikipedia.org/wiki/Parametricity) property
enjoyed by Haskell's type system.  Basically, the type signature says
that the only operations involving type `t` that `allRollsBody` can
use are the ones from the `Num` type class.  [Looking at
them](https://www.stackage.org/haddock/lts-13.21/base-4.12.0.0/Prelude.html#t:Num),
we see that they give us a way to *make* new `t`s from an `Integer`
(`fromInteger`) and ways to *combine* `t`s to give other `t`s (`+`,
`*`, etc.).  On the other hand, there is no way that a value of
another type can be *created from* a `t`.  Therefore, the only way
that an argument of type `t` could be used to affect the result is if
the type variable `t` appears in the type of the result.  The result
has type `[DiceVals]` so it cannot be affected by the argument of type
`t`!

If that seems baffling to you, do not be despondent.  Although
parametricity is an extremely sophisticated property with a complex
proof, using it in practice becomes second nature.  Haskell
programmers use it to great advantage in creating APIs which remain
flexible whilst providing strong guarantees via their type signatures.


### Remove unused argument

One way or another our program transformations have taken us to a
place where we feel comfortable removing the `Integer` argument
without having to think too hard about the justification.  We can
inspect the body and see that is not used in a way that can affect the
result or we can use parametricity to deduce the same thing.  Either
way we can remove the argument.

```haskell
allRolls :: DiceChoice -> DiceState -> [ DiceState ]
allRolls choices (vs, n) = fmap (, n-1) $ allRollsBody choices vs

allRollsBody :: DiceChoice -> DiceVals -> [ DiceVals ]
allRollsBody choices vs = case pop choices vs of
  Nothing -> [ [] ]
  Just ((chosen, v), (choices, vs)) -> do
    roll <- allRollsBody choices vs
    [ d:roll | d <- rollList ]
          where rollList = if chosen then [v] else [ 1..6 ]
```

## Conclusion

In the [earlier article](../good-design-and-type-safety-in-yahtzee/) I
said that "the only way I can suggest that one discovers [that the
argument is unused] is to think through how the the code actually
works ... this is not just a simple refactoring".  I was wrong!  There
is a small sequence of simple transformations that improve the code
whilst at the same time taking us to a place where we easily see that
the argument is unused.  For the latter either we use a small amount
of brainpower to inspect the implementation or we take advantage of
parametricity.

A small amount of Haskell knowledge was required but that is mostly
just because the code is written in Haskell.  Other languages will
have their own particular constructs and equivalent transformations,
although if they lack `case` statements and expression-valued blocks
the transformations might appear a bit more clunky. The only
typed-functional-laguage-specific concept in this article is
parametricity.  In this small example we were happy to just inspect
the body of the function.  Parametricity really shines in more
complicated codebases where unrelated, opaque, pieces of functionality
are being combined.

# Good design and type safety in Yahtzee

Mark Dominus wrote [an article asking how to take advantage of
Haskell's type safety in a simple dice-rolling simulation
function](https://blog.plover.com/prog/haskell/type-markers.html) for
the game Yahtzee.  He added wrapper types so that one cannot
mistakenly apply the function to values that merely have the correct
type "by accident".  He says the result is "unreadable" and
"unmaintainable".  It certainly doesn't look nice!  I'd claim it's not
even of much practical safety benefit (although I suppose that depends
on what the rest of the program looks like).

Mark says

> I don't claim this code is any good; I was just hacking around
  exploring the problem space. But it does do what I wanted.

But we can't just expect to sprinkle type safety on a bad design and
get something good.  Type safety and good design are qualities that
evolve symbiotically.  By using type safety merely to make things
safer for our callers we miss out on a host of benefits.  Type safety
should be used to guide us in the design of our *implementations*,
which our callers never see.  In fact I would argue that Mark is
trying to add type safety in exactly the wrong way.

If there is an interface whose implementation we don't control then
all we can do is to slap a type safe wrapper on it and be done with
it.  That is of some benefit.  On the other hand, if we *do* control
the implementation then the type safe wrapper specifies invariants
that we can take advantage of inside the implementation.  They can
help us *write* the implementation.

We should build type safe structures and combinators relevant to our
domain and implement our solution in terms of those.  Likewise we
should look at our solution and factor out repeated patterns into type
safe structures and combinators relevant to our domain.  This is
symbiotic evolution!

Type safety and good design proceed hand-in-hand along the road of
implementation evolution.  Type safety nudges us in the direction of
good design and good design nudges us in the direction of type safety.
One of the reasons I prefer Haskell to other languages is that the
compiler can enforce the type safety end of this bargain.  On the
other hand, there's no reason a design in Python, say, can't be guided
by "type safety" in the same sense as a design in Haskell.  It's just
that the "type safety" won't be checked by any tooling and so there's
no evolutionary pressure in that direction (Python's recent type
annotations notwithstanding).

Let's evolve Mark's program in the direction of good design and see
what arises.  In this case, as we'll see, I'm not sure there's much to
be gained by trying to "add type safety".  On the other hand, that
there *is* much to be gained by improving the design *guided by*
considerations of type safety.  The design improvements would apply
equally well to an implementation in Python.

I've shown the stages of program evolution as diffs.  Sometimes
they're easy to read and sometimes they're difficult.  If anyone knows
how to get diffs to render nicely in Pandoc (perhaps like GitHub
renders them) then please [contact
me](http://web.jaguarpaw.co.uk/~tom/contact/).

### Original implementation

This is the original implementation, before Mark added wrapper types.
Most of the refactorings that I will perform are independent of what
the implementation actually does; they are simply small changes that
are obviously correct.  Some are not obviously correct unless you know
what the program does, so let's explain that `allRolls` does the
following

* takes `DiceState` (a list of dice values and an `Integer` `n` of
  rolls left to perform), and also
* takes `DiceChoice` (a choice of which dice to reroll), and
* returns a list of all the possible rerolls along with an `Integer`
  of rolls left to perform.

Instead of adding wrapper types let's just try to improve the design
and see what happens.

```haskell
type DiceChoice = [ Bool ]
type DiceVals   = [ Integer ]
type DiceState = (DiceVals, Integer)

allRolls :: DiceChoice -> DiceState -> [ DiceState ]
allRolls [] ([], n) = [ ([], n-1) ]
allRolls [] _ = undefined
allRolls (chosen:choices) (v:vs, n) =
    allRolls choices (vs, n-1) >>=
        \(roll,_) -> [ (d:roll,  n-1) | d <- rollList ]
          where rollList = if chosen then [v] else [ 1..6 ]

example =
  let diceChoices = [ False, True, True, False, False ]
      diceVals = [ 6, 4, 4, 3, 1 ]
  in mapM_ print $ allRolls diceChoices (diceVals, 2)
```

### Explain the invariant

There's a `undefined` in there.  That is always a red flag.  In this
case `undefined` occurs when there are dice we could reroll but we
haven't specifed whether or not to reroll them: the choice list is too
short.  Let's explain that in an error message.  It communicates
better to both the end user of the program and the developer.

We don't need to know what the program does to apply this refactoring
but knowing it does give us more confidence that we are doing the
right thing.

```
 allRolls :: DiceChoice -> DiceState -> [ DiceState ]
 allRolls [] ([], n) = [ ([], n-1) ]
-allRolls [] _ = undefined
+allRolls [] _ =
+  error "Invariant violated: choices must be same length as vals"
 allRolls (chosen:choices) (v:vs, n) =
     allRolls choices (vs, n-1) >>=
         \(roll,_) -> [ (d:roll,  n-1) | d <- rollList ]
```


### Avoid catch-all pattern

I prefer to have as few overlapping patterns as possible, even
"catch-all" (`_`) patterns.  This is a fairly minor change, but let's
do it anyway.  I think it modestly improves the design.

We don't need to know what the program does to apply this refactoring.

```
 allRolls :: DiceChoice -> DiceState -> [ DiceState ]
 allRolls [] ([], n) = [ ([], n-1) ]
-allRolls [] _ =
+allRolls [] (_:_, _) =
   error "Invariant violated: choices must be same length as vals"
```


### Add another invariant check

There's actually a missing pattern (which `-Wall` will pick up).
Let's add it.  Another modest improvement.

Like with the first invariant check, we don't need to know what the
program does to apply this refactoring but knowing it does give us
more confidence that we are doing the right thing.

```
 allRolls :: DiceChoice -> DiceState -> [ DiceState ]
 allRolls [] ([], n) = [ ([], n-1) ]
 allRolls [] (_:_, _) =
   error "Invariant violated: choices must be same length as vals"
+allRolls (_:_) ([], _) =
+  error "Invariant violated: choices must be same length as vals"
```


### Add `pop` function

There are two distinct things that `allRolls` does.

1. It checks the invariant and if the invariant holds extracts
relevant inputs.
2. It runs the algorithm on the relevant inputs.

Let's separate the concerns by adding a `pop` function that does 1.
Note that the interface between `pop` and `allRolls` has type safety!
Once `pop` returns, an invalid state is not possible.  `allRolls` does
not change, except to pass its argument through `pop`.

We don't need to know what the program does to apply this refactoring.

```haskell
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
```

### Indicate that a value is unused

This is the first time we have to apply real thinking to the design
process.  Weirdly, the `n-1` argument to the recursive call to
`allRolls` is not used in the final result.  The only way I can
suggest that one discovers this is to think through how the the code
actually works.  Unlike the above changes, this is not just a simple
refactoring.

Let's indicate that the argument is unused by applying an `error`
instead.  In a language without lazy evaluation you might like to
apply some nonsense value like `-999999999` instead, and check that
the results of the function call are not nonsense!

When we run this we don't get a crash, which implies that that
argument was indeed not used.

```
 allRolls choices (vs, n) = case pop choices vs of
   Nothing -> [ ([], n-1) ]
   Just ((chosen, v), (choices, vs)) ->
-    allRolls choices (vs, n-1) >>=
+    allRolls choices (vs, error "Didn't expect to use") >>=
         \(roll,_) -> [ (d:roll,  n-1) | d <- rollList ]
           where rollList = if chosen then [v] else [ 1..6 ]
```

### Prepare to rearrange arguments

Given the observation above I see no reason to package the `DiceVals`
and the `Integer` together.  Let's prepare to separate them.

We don't need to know what the program does to apply this refactoring.
We just have to observe that the `DiceVals` and the `Integer` are not
really used together.

```
 type DiceChoice = [ Bool ]
 type DiceVals   = [ Integer ]
-type DiceState = (DiceVals, Integer)

...

-allRolls :: DiceChoice -> DiceState -> [ DiceState ]
+allRolls :: DiceChoice
+         -> (DiceVals, Integer)
+         -> [ (DiceVals, Integer) ]
```

### Rearrange arguments

Now let's do the separation of the arguments.  The size of the diff
makes the change seem bigger than it is.  It is merely passing two
arguments instead of a tuple!

We don't need to know what the program does to apply this refactoring.

```
 allRolls :: DiceChoice
-         -> (DiceVals, Integer)
+         -> DiceVals
+         -> Integer
          -> [ (DiceVals, Integer) ]
-allRolls choices (vs, n) = case pop choices vs of
+allRolls choices vs n = case pop choices vs of
   Nothing -> [ ([], n-1) ]
   Just ((chosen, v), (choices, vs)) ->
-    allRolls choices (vs, error "Didn't expect to use") >>=
+    allRolls choices vs (error "Didn't expect to use") >>=
         \(roll,_) -> [ (d:roll,  n-1) | d <- rollList ]
           where rollList = if chosen then [v] else [ 1..6 ]

 example =
   let diceChoices = [ False, True, True, False, False ]
       diceVals = [ 6, 4, 4, 3, 1 ]
-  in mapM_ print $ allRolls diceChoices (diceVals, 2)
+  in mapM_ print $ allRolls diceChoices diceVals 2
```

### Rearrange arguments further

Once we have separated `DiceVals` from the `Integer` we notice that
`DiceChoice` and `DiceVals` seem to naturally belong together.  Again
the diff makes the change look bigger than it is.  We're just passing
`DiceChoice` and `DiceVals` as a tuple rather than two arguments.

We don't need to know what the program does to apply this refactoring.

```
-pop :: DiceChoice
-    -> DiceVals
+pop :: (DiceChoice, DiceVals)
     -> Maybe ((Bool, Integer), (DiceChoice, DiceVals))
-pop [] [] = Nothing
-pop (chosen:choices) (v:vs) = Just ((chosen, v), (choices, vs))
-pop (_:_) [] = error "Invariant violated: missing val"
-pop [] (_:_) = error "Invariant violated: missing choice"
+pop ([], []) = Nothing
+pop (chosen:choices, v:vs) = Just ((chosen, v), (choices, vs))
+pop (_:_, []) = error "Invariant violated: missing val"
+pop ([], _:_) = error "Invariant violated: missing choice"

-allRolls :: DiceChoice
-         -> DiceVals
+allRolls :: (DiceChoice, DiceVals)
          -> Integer
          -> [ (DiceVals, Integer) ]
-allRolls choices vs n = case pop choices vs of
+allRolls (choices, vs) n = case pop (choices, vs) of
   Nothing -> [ ([], n-1) ]
   Just ((chosen, v), (choices, vs)) ->
-    allRolls choices vs (error "Didn't expect to use") >>=
+    allRolls (choices, vs) (error "Didn't expect to use") >>=
         \(roll,_) -> [ (d:roll,  n-1) | d <- rollList ]
           where rollList = if chosen then [v] else [ 1..6 ]

 example =
   let diceChoices = [ False, True, True, False, False ]
       diceVals = [ 6, 4, 4, 3, 1 ]
-  in mapM_ print $ allRolls diceChoices diceVals 2
+  in mapM_ print $ allRolls (diceChoices, diceVals) 2
```

### Avoid unpacking tuple

We no longer need to unpack the tuple!

We don't need to know what the program does to apply this refactoring.

```
-allRolls (choices, vs) n = case pop (choices, vs) of
+allRolls t n = case pop t of
   Nothing -> [ ([], n-1) ]
-  Just ((chosen, v), (choices, vs)) ->
-    allRolls (choices, vs) (error "Didn't expect to use") >>=
+  Just ((chosen, v), t) ->
+    allRolls t (error "Didn't expect to use") >>=
```

### We don't use the `Integer`.  Make this structural.

Given that we have an unused argument in the recursive call let's see
if we can change our design to make this obvious, i.e. make the fact
that we don't use it an essential part of the structure of the
program, not just a property.  In this case it amounts to pairing the
rolls with `n-1` *after* the bulk of the algorithm (`allRollsNoN`) has
finished.

This is the second time we have to actually analyse how our program
works rather than just apply a mechanical translation.

```
 allRolls :: (DiceChoice, DiceVals)
          -> Integer
          -> [ (DiceVals, Integer) ]
-allRolls t n = case pop t of
-  Nothing -> [ ([], n-1) ]
+allRolls t n = [ (vals, n-1) | vals <- allRollsNoN t ]
+
+allRollsNoN :: (DiceChoice, DiceVals) -> [ DiceVals ]
+allRollsNoN t = case pop t of
+  Nothing -> [ [] ]
   Just ((chosen, v), t) ->
-    allRolls t (error "Didn't expect to use") >>=
-        \(roll,_) -> [ (d:roll,  n-1) | d <- rollList ]
+    allRollsNoN t >>=
+        \roll -> [ d:roll | d <- rollList ]
           where rollList = if chosen then [v] else [ 1..6 ]
```

### Introduce a type synonym

Given that `DiceChoice` and `DiceVals` seem to belong together
let's add a type synonym (`DiceTurn`) for that.

We don't need to know what the program does to apply this refactoring.
We just observe that the pair of things are always used together.

```
+type DiceTurn  = (DiceChoice, DiceVals)

-pop :: (DiceChoice, DiceVals)
-    -> Maybe ((Bool, Integer), (DiceChoice, DiceVals))
+pop :: DiceTurn
+    -> Maybe ((Bool, Integer), DiceTurn)
 pop ([], []) = Nothing
 pop (chosen:choices, v:vs) = Just ((chosen, v), (choices, vs))
 pop (_:_, []) = error "Invariant violated: missing val"
 pop ([], _:_) = error "Invariant violated: missing choice"

-allRolls :: (DiceChoice, DiceVals)
+allRolls :: DiceTurn
          -> Integer
          -> [ (DiceVals, Integer) ]
 allRolls t n = [ (vals, n-1) | vals <- allRollsNoN t ]

-allRollsNoN :: (DiceChoice, DiceVals) -> [ DiceVals ]
+allRollsNoN :: DiceTurn -> [ DiceVals ]
```

### Make illegal states unrepresentable

Our invariant is that the number of `DiceChoice`s must be the same as the
number of `DiceVals`.  Semantically, we actually want something
stronger: each of the `DiceChoice`s corresponds to exactly one of the
`DiceVals`.  In my experience this is the single most common
non-trival failure to structurally enforce program behaviour (and [I'm
not the only one to see
it](https://twitter.com/fried_brice/status/1178140883633479680)).

The fix is to put pairs of dice choice and dice vals in the same list!
We can entirely remove our invariant check.  The invariant is enforced
by the type.

Consumers will have to change too but they'll be better off for it!
In `example` I just `zip`ped the args.  It could also be something
much better.

```
-type DiceChoice = [ Bool ]
 type DiceVals   = [ Integer ]
-type DiceTurn  = (DiceChoice, DiceVals)
+type DiceTurn  = [(Bool, Integer)]

 pop :: DiceTurn
     -> Maybe ((Bool, Integer), DiceTurn)
-pop ([], []) = Nothing
-pop (chosen:choices, v:vs) = Just ((chosen, v), (choices, vs))
-pop (_:_, []) = error "Invariant violated: missing val"
-pop ([], _:_) = error "Invariant violated: missing choice"
+pop [] = Nothing
+pop (a:as) = Just (a, as)

 allRolls :: DiceTurn
          -> Integer
@@ -25,4 +22,4 @@ allRollsNoN t = case pop t of
 example =
   let diceChoices = [ False, True, True, False, False ]
       diceVals = [ 6, 4, 4, 3, 1 ]
-  in mapM_ print $ allRolls (diceChoices, diceVals) 2
+  in mapM_ print $ allRolls (zip diceChoices diceVals) 2
```

### Use `uncons`

Having done that we see that `pop` is just the standard function
"`uncons`".

We don't need to know what the program does to apply this refactoring.

```
+import Data.List (uncons)
+
 type DiceVals   = [ Integer ]
 type DiceTurn  = [(Bool, Integer)]

-pop :: DiceTurn
-    -> Maybe ((Bool, Integer), DiceTurn)
-pop [] = Nothing
-pop (a:as) = Just (a, as)
-
 allRolls :: DiceTurn
          -> Integer
          -> [ (DiceVals, Integer) ]
 allRolls t n = [ (vals, n-1) | vals <- allRollsNoN t ]

 allRollsNoN :: DiceTurn -> [ DiceVals ]
-allRollsNoN t = case pop t of
+allRollsNoN t = case uncons t of
```

### Don't need uncons

Having said that, we don't actually need `uncons`.  We can just
pattern match directly.

We don't need to know what the program does to apply this refactoring.

```
 allRollsNoN :: DiceTurn -> [ DiceVals ]
-allRollsNoN t = case uncons t of
-  Nothing -> [ [] ]
-  Just ((chosen, v), t) ->
+allRollsNoN t = case t of
+  [] -> [ [] ]
+  (chosen, v):t ->
```

### Use do notation

The use of the bind operator (`>>=`) and list comprehension are not
particularly clear.  Let's rewrite it to use do notation.  (In fact I
recommend defaulting to do notation over operators unless there's some
compelling readability benefit to using the latter.)

We don't need to know what the program does to apply this refactoring.

```
 allRollsNoN :: DiceTurn -> [ DiceVals ]
 allRollsNoN t = case t of
   [] -> [ [] ]
-  (chosen, v):t ->
-    allRollsNoN t >>=
-        \roll -> [ d:roll | d <- rollList ]
+  (chosen, v):t -> do
+    roll <- allRollsNoN t
+    d    <- rollList
+    [ d:roll ]
```

### Prepare for `mapM`

We can see from the above that what our program does is takes the head
of a list, runs recursively on the tail, does something to the head,
and then puts it back on the tail.  This is a "map" operation.
Specifically in this case we are mapping in a monad so we use `mapM`.
In modern Haskell you'd use `traverse`, but I'm going to stick to
`mapM` because `traverse` does not read so well.  (In my opinion `traverse` really
ought to be called `mapA` but [others don't like the idea of that
change](https://www.reddit.com/r/haskell/comments/68w09h/proposal_to_add_mapa_as_synonym_for_traverse/).)

In this change we just make the function we are mapping take explicit
arguments.  We'll switch to use `mapM` in the next change.

We don't need to know what the program does to apply this refactoring.

```
   (chosen, v):t -> do
     roll <- allRollsNoN t
-    d    <- rollList
+    d    <- rollList (chosen, v)
     [ d:roll ]
-          where rollList = if chosen then [v] else [ 1..6 ]
+          where rollList (chosen, v)
+                    = if chosen then [v] else [ 1..6 ]
```


### Use `mapM`

Now we can just use `mapM` directly.

We don't need to know what the program does to apply this refactoring
but we do need to know the general concept of "mapping" and the
specific implementation `mapM`.  Be careful!  This particular
"refactoring" actually reverses the order of effects -- the list of
dice rolls will come out in a different order.

```
-allRollsNoN t = case t of
-  [] -> [ [] ]
-  (chosen, v):t -> do
-    roll <- allRollsNoN t
-    d    <- rollList (chosen, v)
-    [ d:roll ]
-          where rollList (chosen, v)
-                    = if chosen then [v] else [ 1..6 ]
+allRollsNoN =
+    mapM (\(chosen, v) -> if chosen then [v] else [ 1..6 ])
```


### Avoid boolean blindness

There's ambiguity in the type `DiceTurn = [(Bool, Integer)]`.  Does
the `Bool` refer to whether we keep the dice or to whether we reroll
them?  There's no way for me to tell without seeing this conditional
inside the function:

```
if chosen then [v] else [ 1..6 ]
```

Ah, so the `Bool` refers to whether we *keep* the dice.  Perhaps this
is written in the documentation somewhere, but why do I believe that
the documentation is kept in line with the implementation?  I want a
single source of truth!

Let's add a type to avoid "boolean blindness".  The type of
`allRollsBetter` still does not guarantee that the implementation does
the right thing with its argument but it does make any deviation
glaringly obvious.

We don't need to know what the program does to apply this refactoring.
It requires the uncontroversial `LambdaCase` language extension.

```
-allRollsNoN =
-    mapM (\(chosen, v) -> if chosen then [v] else [ 1..6 ])
+allRollsNoN = allRollsBetter . map fromTurn
+
+data DiceChoice = Keep Integer | Reroll
+
+fromTurn :: (Bool, Integer) -> DiceChoice
+fromTurn (chosen, v) = if chosen then Keep v else Reroll
+
+allRollsBetter :: [DiceChoice] -> [ DiceVals ]
+allRollsBetter = mapM $ \case
+  Reroll -> [ 1..6 ]
+  Keep v -> [v]
```

### Keep the better version

Let's get rid of all vestiges of the old version.  What `allRolls`
does could be done more clearly at the call site.  At this point I
wouldn't add wrapper types for "type safety".  The rest of the program
might be sufficiently complex that they would help, but they certainly
don't add anything in this simple example.

The new version communicates *much* better.  We "map" over our
`DiceVals` list, that is, apply a function to each element in turn.
In this case we're taking advantage of the `Monad` instance for lists,
so we use `mapM`.  The function we map simply says

* Do we want to `Reroll`?  If so, the possible results are `[1..6]`
* Do we want to `Keep v`?  If so, the possible results are just
`[v]`"


```haskell
{-# LANGUAGE LambdaCase #-}

type DiceVals   = [Integer]
data DiceChoice = Keep Integer | Reroll

allRollsBetter :: [DiceChoice] -> [DiceVals]
allRollsBetter = mapM $ \case
  Reroll -> [1..6]
  Keep v -> [v]

example =
  let diceVals = [ Reroll, Keep 4, Keep 4, Reroll, Reroll ]
  in mapM_ print $ allRollsBetter diceVals
```

Look at `allRollsBetter` in comparison to the original `allRolls`!

```haskell
allRolls :: DiceChoice -> DiceState -> [ DiceState ]
allRolls [] ([], n) = [ ([], n-1) ]
allRolls [] _ = undefined
allRolls (chosen:choices) (v:vs, n) =
    allRolls choices (vs, n-1) >>=
        \(roll,_) -> [ (d:roll,  n-1) | d <- rollList ]
          where rollList = if chosen then [v] else [ 1..6 ]
```

How did we end up with something so much clearer?  We applied a
sequence of transformations to improve the design, almost all of which
are applicable in any language.  The transformations were partly
informed by a notion of "type safety".  Specifically, we aimed to
model our domain using types and functions that make illegal states
unrepresentable.

None of this *requires* a language like Haskell.  It would be good
design in Python as well.  One of Python's weaknesses is that it makes
dealing with sum types awkward.  We would have had to take a slightly
different approach for the `Maybe` returned by `pop` (probably `None`
or a tuple), the `DiceChoice` type (probably a pair of classes) and
the list monad (probably just a recursive generator function).
Ultimately though, the benefit of Haskell is not that it allows us to
implement well-typed designs, nor particularly that it forbids us from
implementing ill-typed designs.  The benefit is that it nudges us away
from poorly-typed, poorly-structured designs *and holds our hand as it
does so*.

# Plucking constraints in Bluefin

-- or, "Handling effects in Bluefin"

One of the promises of strongly-typed, pure functional programming is
to "make invalid states unrepresentable".  We can broaden the slogan
to "make invalid *behaviours* unrepresentable".  Indeed, that's one of
the purposes of effect systems in Haskell: to circumscribe an
operation's range of observable behaviours.  Typically, effect systems
provide a way of "handling" effects, that is, removing effects from
the range of observable behaviours.  Matt Parsons calls the technique
for handling effects in the "MTL effect system" "[plucking
constraints](https://www.parsonsmatt.org/2020/01/03/plucking_constraints.html)".

The [Bluefin](https://hackage.haskell.org/package/bluefin) effect
system, like MTL, uses a system of constraints to track effects and
allows effects to be handled, reducing the range of observable
behaviours.  Let's have a look at how that works, after first looking
at how it works in transformers and MTL, for comparison.

## Transformers

How does handling effects look in the context of
[transformers](https://hackage.haskell.org/package/transformers) in
Haskell?  Consider the operation

```.hs
operTrans :: ExceptT String (State Int) ()
```

The possible behaviour of such an operation is "manipulating an `Int`
state, eventually terminating with a `()` unless a `String` exception
was thrown".  So far so good.  But the story gets even better: we can
*handle* effects, to reduce the range of valid behaviour.  For
example, we could choose to handle the exception by adding the length
of the thrown `String` to the `Int` state, as follows:

```.hs
handleLengthTrans ::
  ExceptT String (State Int) () -> State Int ()
handleLengthTrans m = do
  e <- runExceptT m
  case e of
    Left s -> modify (+ length s)
    Right () -> pure ()
```

Then `handleLengthTrans operTrans :: State Int ()`. We have reduced
the range of possible behaviour to "manipulate an `Int` state but do
not (observably) throw an exception" (despite the fact that the
_implementation_ uses an exception).

## MTL

Typically, however, we don't want to write concrete monad transformer
stacks.  One alternative is MTL-style constraints, for example:

```.hs
operMTL ::
  (MonadError String m, MonadState Int m) => m ()
```

Exactly the same implementation as `handleLengthTrans` works for this
MTL-style function too, although the type is an MTL-style one.

```.hs
handleLengthMTL ::
  MonadState Int m => ExceptT String m () -> m ()
handleLengthMTL m = do
  e <- runExceptT m
  case e of
    Left s -> modify (+ length s)
    Right () -> pure ()
```

Then `handleLengthMTL operMTL :: MonadState Int m => m ()`.  Again, the
possible behaviour is "manipulate an `Int` state but do not
(observably) throw an exception".

How does this business with `MonadError` and `MonadState` work to
allow us to handle an effect?  Well, that's the technique that Matt
calls "plucking constraints".  Let's consider the two types involved,
renaming the type variable in `operMTL` from `m` to `m'` to avoid
confusion.

```.hs
operMTL ::
  (MonadError String m', MonadState Int m') => m' ()
handleLengthMTL ::
  MonadState Int m => ExceptT String m () => m ()
```

Then for `handleLengthMTL operMTL` to type check we need `m'` to be
`ExceptT String m`. We end up with

```.hs
handleLengthMTL operMTL ::
  ( MonadState Int m,
    MonadState Int (ExceptT String m),
    MonadError String (ExceptT String m)
  ) =>
  m ()
```

That's exactly what we anticipate, except for the constraints.  We
have three constraints where we expected one.  That's OK though: we
can discharge the unexpected constraints to obtain just the expected
one.  Let's see how that proceeds.

The first constraint is `MonadState Int m`, and there's nothing to be
done about that.  It's still a constraint on the result.  To discharge
the second constraint the compiler uses the instance

```.hs
instance MonadState s m => MonadState s (ExceptT e m)
```

so the second constraint reduces to `MonadState Int m`, identical to
the constraint we already have.  Thirdly, there is an instance

```.hs
instance Monad m => MonadError e (ExceptT e m)
```

so the third constraint reduces to `Monad m`.  But `MonadState Int m`
[can only hold when `Monad m` already
holds](https://www.stackage.org/haddock/lts-22.35/mtl-2.3.1/Control-Monad-State-Class.html#t:MonadState),
so the single remaining constraint is just `MonadState Int m`.

This behaviour crucially depended on two elements: firstly, the
`MonadError` instance for `ExceptT`.  That part is straightforward and
expected.  The second element is less straightforward: the
`MonadState` instance that "passes" state behaviour through the
`ExceptT` layer to the inner `m`, which is itself required to be an
instance of `MonadState`.

This is why MTL-style has the "*n* squared instances problem": for
every effect "MyEffect" that I define, I need a constraint (say
`MonadMyEffect`), I need a transformer (say `MyEffectT`), a
`MonadMyEffect` instance for `MyEffectT`, and then crucially, *n* more
instances one for every other effect constraint, that pass the other
effect constraint through `MyEffectT` to the inner monad.  For
example, I need

```.hs
instance MonadState s m => MonadState s (MyEffectT m)
instance MonadError e m => MonadError e (MyEffectT m)
...
```

So far this article has mostly recapitulated what Matt Parsons wrote
in "[Plucking
Constraints](https://www.parsonsmatt.org/2020/01/03/plucking_constraints.html)".
Now let's see something different: how plucking constraints works in
Bluefin.

## Bluefin

We have been working with an operation that can manipulate an `Int`
state and throw a `String` exception specified in transformers style
and MTL-style, respectively:

```.hs
operTrans ::
  ExceptT String (State Int)
operMTL ::
  (MonadError String m, MonadState Int m) => m ()
```

The Bluefin equivalent of the operation is

```.hs
operBf ::
  (e1 :> es, e2 :> es) =>
  State Int e1 ->
  Exception String e2 ->
  Eff es ()
```

and the Bluefin equivalent of the handler which handles, and therefore
removes, the exception effect is

```.hs
handleLengthBf ::
  (e1 :> es) =>
  (forall e2. Exception String e2 -> Eff (e2 :& es) r) ->
  State Int e1 ->
  Eff es ()
handleLengthBf m st =
  e <- try m
  case e of
    Left s -> modify st (+ length s)
    Right () -> pure ()
```

When we use it to handle `operBf` we get

```.hs
handledOper :: (e1 :> es) => State Int e1 -> Eff es ()
handledOper = handleLengthBf (operBf s) s
```

Let's see how this one works.  Again, I'm going to rename a type
variable to avoid confusion, this time `es` in `operBf` to `es'`.

```.hs
operBf ::
  (e1 :> es', e2 :> es') =>
  State Int e1 ->
  Exception String e2 ->
  Eff es' ()
```

Subsequently, for `handleLengthBf (operBf s) s` to type check we need
`es'` to be `e2 :& es`, so we get the type

```.hs
handledBf :: (e1 :> es) => State Int e1 -> Eff es ()
handledBf =
  handleLengthbf
    (operBf s ::
      forall e2.
      (e1 :> (e2 :& es), e2 :> (e2 :& es)) =>
      Exception String e2 ->
      Eff (e2 :& es) ()
   )
```

Notice that I have not only annotated `handleBf` with a type, but I
have also annotated `operBf`. That's because the constraints that we
need to resolve contain `e2`, a type variable bound by the type of
`operBf`.  An interesting development!  Nothing like this was seen in
the MTL-style example.

The constraints we must deal with involve [Bluefin's `:>`
operator](https://hackage.haskell.org/package/bluefin-0.0.7.0/docs/Bluefin-Eff.html#t::-62-).
They are:

```.hs
e1 :> es
e1 :> (e2 :& es)
e2 :> (e2 :& es)
```

The first, `e1 :> es` is easiest, because there's nothing we can do
about it: it remains in the final form.  This is analogous to the case
above of `MonadState Int m`.  The second, `e1 :> (e2 :& es)`, is dealt
with by the following instance for `:>`:

```.hs
instance (e :> es) => e :> (x :& es)
```

It becomes `e1 :> es`, identical to the constraint we already have.
This is analogous to the case above of:

```.hs
MonadState s m => MonadState s (ExceptT e m)
```

The third, `e2 :> (e2 :& es)`, is satisfied by:

```.hs
instance e :> (e :> es)
```

and so is absent from the final form.  This is analogous to the case
of `MonadError String (ExceptT String m')` above.

In this manner, Bluefin avoids the "*n* squared instances" problem:
the only instances needed are those of `:>`. They are defined once and
for all to deal with all the situations that will arise.

## Incoherent instances

But hold on!  Something funny happened here.  The third constraint,
`e2 :> (e2 :& es)`, matches *both* of these instance definitions:

<a name="bluefin-instances"></a>

```.hs
instance e :> (e :> es)
instance (e :> es) => e :> (x :& es)
```

Above we said that the constraint was satisfied by the former
instance: a desirable outcome.  On the other hand, the latter instance
seems to suggest the constraint should resolve to `e2 :> es`: an
undesirable outcome because, since `e2` was bound by the type of
`operBf`, it can't be satisfied!

What's going on?  Well, I omitted something from the instance
declarations above.  The declaration for `e :> (e :& es)` is actually
*incoherent*:

```.hs
instance {-# INCOHERENT #-} e :> (e :& es)
instance (e :> es) => e :> (x :& es)
```

The [instance resolution rules for incoherent
instances](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#overlapping-instances)
(and for overlapping instances, which are a weaker form of
incoherence) are subtle, and I won't go into details in this article
about how exactly they work in this case, but I will state that they
allow the constraint `e2 :> (e2 :> es)` to be dispatched whilst also
allowing the constraint `e1 :> (e2 :> es)` to resolve to `e1 :> es`,
which is exactly what type inference of Bluefin handlers needs!

### Incoherence questions

The use of incoherent instances raises questions:

1. Would overlapping instances be sufficient?
2. Can it lead to type safety violations?

Regarding question 1: no, overlapping instances would not be
sufficient.  The explanation involves subtleties of how instance
resolution works for incoherent instances compared to overlapping
instances and I won't go into further details in this article.

Regarding question 2: no, using incoherent instances cannot lead to
violations of type safety in Bluefin. [The "axiomatic"
instances](#bluefin-instances) are valid, so any instance derived from
them is valid regardless of *how* it is derived.  Furthermore, the
`(:>)` constraint contains no run time data so there can be no
difference in behaviour arising from how the instance was derived.
Therefore, although Bluefin's use of incoherent instances can lead to
a failure of type checking or type inference (a valid program can be
rejected), it cannot lead to a failure of type safety (an invalid
program cannot be accepted) nor unexpected behaviour.

## Conclusion and commentary

Haskell effect systems allow making invalid behaviour unrepresentable.
If you define a new effect in MTL-style then you'll be required to do
extra work for each other effect you want to be compatible with (the
"*n* squared instances problem). If you define a new effect in
Bluefin-style you won't: Bluefin's `:>` constraint and associated
instances allow arbitrary effects to be combined with no extra work.

Similar effect systems, such as polysemy and effectful have the same
benefits as Bluefin.  Indeed, effectful has [a similar system of
instances](https://hackage.haskell.org/package/effectful-core-2.4.0.0/docs/Effectful.html#t::-62-)
to Bluefin, where the instances themselves have no run time relevance,
because Bluefin's system was derived from effectful's.

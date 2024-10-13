# Plucking constraints in Bluefin

One of the promises of strongly-typed, pure functional programming is
to "make invalid states unrepresentable".  In fact we can broaden the
notion to "make invalid *behaviour* unrepresentable".  How does that
look in the context of effects in Haskell?  Consider

```.hs
foo :: ExceptT String (State Int) ()
```

The valid behaviours of such an operation are "manipulating an `Int`
state, eventually terminating with a `()` unless a `String` exception
was thrown".  So far so good.  But the story gets even better.  We can
*handle* effects, to reduce the range of valid behaviours.  For
example, we could choose to handle the `String` exception, adding its
length to the `Int` state, as follows

```.hs
handleLength ::
  ExceptT String (State Int) () -> State Int ()
handleLength m = do
  e <- runExceptT m
  case e of
    Left s -> modify (+ length s)
    Right () -> pure ()
```

Then `handleLength foo :: State Int ()`. We have reduced the range of
valid behaviours to those operations that manipulate an `Int` state
but do not (observably) throw an exception (despite the fact that the
_implementation_ uses an exception).

Typically, however, we don't want to write concrete monad transformer
stacks.  One alternative is MTL-style constraints, for example

```.hs
fooMTL ::
  (MonadError String m, MonadState Int m) => m ()
```

Then exactly the same implementation as `handleLength` works for this
MTL style function too, although the type is an MTL-style one.

```.hs
handleLengthMTL ::
  MonadState Int m => ExceptT String m () -> m ()
handleLengthMTL m = do
  e <- runExceptT m
  case e of
    Left s -> modify (+ length s)
    Right () -> pure ()
```

Then `handleLengthMTL fooMTL :: MonadState Int m => m ()`.  Again, the
possible behaviours are those that manipulate an `Int` state but do
not (observably) throw an exception.

How does this business with `MonadState` work?  Well, let's consider
the two types involved, renaming the type variable in `fooMTL` from
`m` to `m'` to avoid confusion.

```.hs
fooMTL ::
  (MonadError String m', MonadState Int m') => m' ()
handleLengthMTL ::
  MonadState Int m => ExceptT String m () => m ()
```

Then for `handleLengthMTL fooMTL` to type check we need `m'` to be
`ExceptT String m`. We end up with

```.hs
handleLengthMTL fooTML ::
  ( MonadError String (ExceptT String m'),
    MonadState Int (ExceptT String m'),
    MonadState Int m,
  ) =>
  ExceptT String m' () ->
  m' ()
```

That's exactly what we anticipate, except for the constraints.  We
have three constraints where we expected one.  That's OK though: we
can discharge the unexpected constraints to obtain just the expected
one.  Let's see how that proceeds.  Firstly, there is an instance

```.hs
instance Monad m => MonadError e (ExceptT e m)
```

so the first constraint reduces to `Monad m`.  The third constraint is
`MonadState Int m`, and there's nothing to be done about that.  It's
still a constraint on the result.


To discharge the second constraint the compiler uses the instance

```.hs
instance MonadState s m => MonadState s (ExceptT e m)
```

so the second constraint reduces to `MonadState Int m`.  But
`MonadState Int m` [can only hold when `Monad m` already
holds](https://www.stackage.org/haddock/lts-22.35/mtl-2.3.1/Control-Monad-State-Class.html#t:MonadState),
so the single remaining constraint is just `MonadState Int m`.

This behaviour crucially depended on two elements: firstly, the
`MonadError` instance for `ExceptT`.  That part is straightforward and
expected.  The second element is less straightforward: the
`MonadState` instance that "passes" state behaviour through the
`ExceptT` layer to the inner `m`, which is itself required to be an
instance of `MonadState`.

This is why MTL style has the "n squared instances problem": for every
effect "MyEffect" that I define, I need a constraint (say
`MonadMyEffect`), I need a transformer (say `MyEffectT`), a
`MonadMyEffect` instance for `MyEffectT`, and then crucially, n more
instances one for every other effect constraint, that delegate the
other effect constraint to the inner monad.  For example, I need

```.hs
instance MonadState s m => MonadState s (MyEffectT m)
instance MonadError e m => MonadError e (MyEffectT m)
...
```

So far this article has mostly recapitulated what Matt Parsons wrote
in his article "[Plucking
Constraints](https://www.parsonsmatt.org/2020/01/03/plucking_constraints.html)".
Now let's see something different: how plucking constraints works in
Bluefin.

## Bluefin

We have been working with an operation that can manipulate an `Int`
state and throw a `String` exception specified in transformers style
and MTL style, respectively

```.hs
foo ::
  ExceptT String (State Int)
fooMTL ::
  (MonadError String m, MonadState Int m) => m ()
```

The Bluefin equivalent of the operation is

```.hs
fooBf ::
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

When we use it to handle `fooBf` we get

```.hs
handledFoo :: (e1 :> es) => State Int e1 -> Eff es ()
handledFoo = handleLengthBf (fooBf s) s
```

Let's see how this one works.  Again, I'm going to rename a type
variable to avoid confusion, this time `es` in `fooBf` to `es'`.

```.hs
fooBf ::
  (e1 :> es', e2 :> es') =>
  State Int e1 ->
  Exception String e2 ->
  Eff es' ()
```

Subsequently, when we form `handleLengthBf (fooBf s) s` we need to
choose `es'` to be `e2 :& es`, so we get the type

```.hs
handledBf :: (e1 :> es) => State Int e1 -> Eff es ()
handledBf =
  handleLengthbf
    (fooBf s ::
      forall e2.
      (e1 :> (e2 :& es), e2 :> (e2 :& es)) =>
      Exception String e2 ->
      Eff (e2 :& es) ()
   )
```

Notice that I have not only annotated `handleBf` with a type, but I
have also annotated `fooBf`. That's because the constraints that we
need to resolve contain `e2`, a type variable bound by the type of
`fooBf`.  An interesting development!

The constraints we have to deal with involve [Bluefin's `:>`
operator](https://hackage.haskell.org/package/bluefin-0.0.7.0/docs/Bluefin-Eff.html#t::-62-).
They are

```.hs
e1 :> es
e1 :> (e2 :& es)
e2 :> (e2 :& es)
```

The first, `e1 :> es` is easiest, because there's nothing we can do
about it.  It remains in the final form.  This is analogous to the
case above of `MonadState Int m`.

The second, `e1 :> (e2 :& es)` is dealt with by the following instance
for `:>`:

```.hs
instance (e :> es) => e :> (x :& es)
```

It becomes `e1 :> es`, identical to the constraint we already have.
This is analogous to the case above of

```.hs
MonadState s m => MonadState s (ExceptT e m)
```

The third, `e2 :> (e2 :& es)`, is satisfied by

```.hs
instance e :> (e :> es)
```

and so is absent from the final form.  This is analogous to the case
of `MonadError String (ExceptT String m')` above.

In this manner, Bluefin avoids the "n squared instances" problem: the
only instances needed are those of `:>`, and they are defined once and
for all to deal with all the situations that they need to.

## Incoherent instances

But hold on!  Something funny happened here.  The third constraint
matches *both* of these instance definitions:

```.hs
instance e :> (e :> es)
instance (e :> es) => e :> (x :& es)
```

Above we said that the third constraint, `e2 :> (e2 :> es)`, was
satisfied by the former: a desirable outcome.  On the other hand, the
latter seems to suggest the third constraint should resolve to `e2 :>
es`: an undesirable outcome because we can't satisfy that constraint!

So what's going on?  Well, I omitted something from the instance
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
allowing the constraint `e1 :> (e2 :> es)` to be satisfied by `e1 :>
es`, which is exactly what type inference of Bluefin handlers needs!

### Incoherence questions

This raises some more questions

1. Would overlapping instances be sufficient?
2. Can this lead to type safety violations?

I'll answer 1 but not explain it: no, overlapping instances are not
sufficient.  The explanation involves subtleties of how instance
resolution works for overlapping instances, and I won't go into
details of that in this article.

The `(:>)` constraint contains no run time data and is only used for
type checking purposes.  Both instances above are correct from the
point of view of type checking, so even if the wrong one was picked,
the worst that can happen is that type checking fails.  There can be
no type safety violation.  Such a choice might cause a valid program
to be rejected but cannot cause an invalid programs to be accepted.

# References

* <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#instance-resolution>

* <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#extension-IncoherentInstances>

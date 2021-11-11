# Make invalid laziness unrepresentable

*Making invalid laziness unrepresentable, even in nested data
structures.*

## The problem

### Lazy and strict data

One circumstance in which [thunk
leaks](http://blog.ezyang.com/2011/05/space-leak-zoo/comment-page-1/)
occur in Haskell programs is when a lazy field of a data type remains
unevaluated for longer than expected.  By way of example consider the
following data type:

```haskell
data Foo = Foo Int Bool
```

During execution of a program what possible states can a value of
type `Foo` be in?  There are broadly five:

1. `<thunk>`
2. `Foo <evaluated Int> <evaluated Bool>`
3. `Foo <evaluated Int> <thunk>`
4. `Foo <thunk> <evaluated Bool>`
5. `Foo <thunk> <thunk>`

The `<thunk>`s can be arbitrarily large run time data
structures! Their unexpected occurrence can cause thunk leaks.
What can we do about that?  When programming in a strongly typed
language we aim to "make invalid states unrepresentable"[^2] so when
we define a data type like `Foo` we should consider which are its
valid states.  Haskell is a lazy language so we cannot forbid
state 1[^1], but are states like 3, 4 and 5 valid?  Perhaps.  We should
carefully consider our use case; but if not (and it's more likely
not than so) then we should forbid those states statically.  How
can we do so?  We can add strictness annotations thus:

```haskell
data FooStrict = FooStrict !Int !Bool
```

(or by enabling `StrictData` which brutally applies the same
transformation to *every* data type definition, or `Strict` which
is even more brutal). `FooStrict` has only two possible states:

1. `<thunk>`
2. `Foo <evaluated Int> <evaluated Bool>`

We have "made invalid laziness unrepresentable".  Much better!

[^1]: barring unlifted data types, which are out of scope for this
discussion and this library.

[^2]: I believe that Yaron Minsky coined the phrase "make illegal
states unrepresentable", in [The Monad.Reader, Issue 7 (April 30,
2007)](https://wiki.haskell.org/wikiupload/0/03/TMR-Issue7.pdf).
I mildly prefer talking about "valid" states than "legal" ones.

### Nested strict data

But the above technique is not particularly general.  Consider

```haskell
data Bar = Bar !(Int, Bool) !(Maybe Double)
```

Despite the strictness annotations this type has many
possible states:

1. `<thunk>`
2. `Bar (<thunk>, <thunk>) Nothing`
3. `Bar (<evaulated Int>, <thunk>) Nothing`
4. `Bar (<thunk>, <evaluated Bool>) Nothing`
5. `Bar (<evaluated Int>, <evaluated Bool>) Nothing`
6. `Bar (<thunk>, <thunk>) (Just <thunk>)`
7. `Bar (<evaluated Int>, <thunk>) (Just <thunk>)`
8. `Bar (<thunk>, <evaluated Bool>) (Just <thunk>)`
9. `Bar (<evaluated Int>, <evaluated Bool>) (Just <thunk>)`
10. `Bar (<thunk>, <thunk>) (Just <evaluated Double>)`
11. `Bar (<evaluated Int>, <thunk>) (Just <evaluated Double>)`
12. `Bar (<thunk>, <evaluated Bool>) (Just <evaluated Double>)`
13. `Bar (<evaluated Int>, <evaluated Bool>) (Just <evaluated Double>)`

Plenty of thunks for leaks to hide in!  Perhaps for some use
cases all the above states are valid but in most cases it is
overwhelmingly likely that only the following states are valid:

1. `<thunk>` (because we can't do anything about it anyway)
2. `Bar (<evaluated Int>, <evaluated Bool>) Nothing`
3. `Bar (<evaluated Int>, <evaluated Bool>) (Just <evaluated Double>)`

No clever application of strictness annotations can restrict us to
this set of states!  The problem is that there's no way of "applying
strictness inside" the nested data types.  How can we make invalid
laziness unrepresentable in nested data types?

## The solution

Since we cannot "apply strictness inside" the nested data type we need
to use separate strict types for those fields of `Bar`.  The usual
approach is to write specific strict data definitions, for example

```haskell
data StrictPair a b = StrictPair !a !b
data StrictMaybe a = StrictNothing | StrictJust !a
```

but I'm going to show you a more lightweight approach.  The library
[`strict-wrapper`](https://hackage.haskell.org/package/strict-wrapper-0.0.0.0/docs/Data-Strict-Wrapper.html)
allows us to rewrite `Bar` as

```haskell
data BarStrict = BarStrict !(Strict (Int, Bool))
                           !(Strict (Maybe Double))
```

Having done so only the valid states are representable:

1. `<thunk>`
2. ```
BarStrict (Strict (<evaluated Int>, <evaluated Bool>))
          (Strict Nothing)
```
3. ```
BarStrict (Strict (<evaluated Int>, <evaluated Bool>))
          (Strict (Just <evaluated Double>))
```

Deeper nesting works too, for example:

```haskell
data Baz = Baz !(Strict (Int, Strict (Either Bool Int)))
               !(Strict (Maybe Double))
```

`Strict` also works well as a wrapper for types whose values will
be passed as strict function arguments.  For example, the
following function has a space leak:

```haskell
example_leak = foldl' f (0, 0) [1..1000]
    where f :: (Int, Int) -> Int -> (Int, Int)
          f (n, s) i = (n + 1, s + i)
```

Manually adding strictness avoids the space leak:

```haskell
example_manual = foldl' f (0, 0) [1..1000]
    where f :: (Int, Int) -> Int -> (Int, Int)
          f (!n, !s) i = (n + 1, s + i)
```

but using `Strict` avoids the space leak in a way that is both
syntactically convenient *and* reflected in the type of the
accumulator function `f`, that is to say, invalid laziness in the
accumulator argument has been made unrepresentable!

```haskell
example_Strict = Data.List.foldl' f (Strict (0, 0)) [1..1000]
    where f :: Strict (Int, Int) -> Int -> Strict (Int, Int)
          f (Strict (n, s)) i = Strict (n + 1, s + i)
```

(Unfortunately we still have to remember to use `foldl'` rather than
`foldl`.  Nothing in the type system can help us there.)

## The API

[`strict-wrapper`](https://hackage.haskell.org/package/strict-wrapper-0.0.0.0/docs/Data-Strict-Wrapper.html)
is implemented using a data family (`Strict`) which maps basic
types to their strict versions.

```haskell
data instance Strict (a, b)    = StrictPair !a !b
data instance Strict (Maybe a) = StrictNothing | StrictJust !a
```

To use
[`strict-wrapper`](https://hackage.haskell.org/package/strict-wrapper-0.0.0.0/docs/Data-Strict-Wrapper.html)
all that you need is the data family `Strict` and the bidirectional
pattern synonym, also called `Strict`. For example, instead of using
`StrictPair a b` as defined above, use `Strict (a, b)`. To create a
`Strict (a, b)` wrap an `(a, b)` in the `Strict` constructor; to
extract an `(a, b)`, pattern match with `Strict`.


## The alternatives

There are various alternatives to
[`strict-wrapper`](https://hackage.haskell.org/package/strict-wrapper-0.0.0.0/docs/Data-Strict-Wrapper.html)
worth considering although in most cases they are too heavyweight or
don't fulfil the same role.

### `seq`/bang patterns

It is always possible to use `seq` (or equivalently bang patterns) to
ensure that invalid thunk states don't arise.  After all, strictness
annotations and strict data types are implemented merely by automatic
insertion of `seq`!  However, in pratice it is extremely difficult to
maintain the level of discipline required to make sure that the `seq`
calls or bang patterns are inserted in the correct places (and not in
the incorrect places).  The benefit of programming in a strongly typed
functional language is that we can make invalid states
unrepresentable.  That principle applies as much to invalid data type
laziness as to other invalid states.

### strict

[`strict`](https://hackage.haskell.org/package/strict) is a library
that provides a grab-bag of features related to strictness: strict
versions of basic types, strict I/O, and a class to map between strict
and lazy types (including `ByteString` and `Text` types and monad
transformer types).

By contrast, `strict-wrapper` is a much smaller and more coherent
subset of the features of `strict`: it only provides strict versions
of basic types and a class to map between them.  In return for being
more restrictive the mapping can be made almost zero-cost (see
"[Efficiency
considerations](https://hackage.haskell.org/package/strict-wrapper-0.0.0.0/docs/Data-Strict-Wrapper.html#g:6)").
Furthermore the `Strict` data family avoids the need for a whole
universe of new strict type names and the `Strict` pattern/constructor
is more ergonomic than `toStrict`/`toLazy` mapping functions.

### deepseq

[`deepseq`](https://hackage.haskell.org/package/deepseq-1.4.5.0/docs/Control-DeepSeq.html
deepseq) is an extremely expensive and blunt hammer.  It has to walk
your entire data structure evaluating any thunks it encounters.  Were
those thunks actually part of a valid state of your program?  In many
(perhaps most) cases they were not!  In those cases would it not be
better to design those thunks out of your data structures and avoid
deepseq entirely?

### nothunks

[nothunks](https://hackage.haskell.org/package/nothunks-0.1.3/docs/NoThunks-Class.html
nothunks) is a debugging tool that allows inspecting a value at run
time to see if it contains any thunks.  That is, it can check at run
time whether a value is invalid.  But if you can make the invalid
laziness *unrepresentable* then why not do so?

## Commentary and conclusion

Thunk leaks are a major cause of unpredictable memory usage in Haskell
programs; a common cause of thunk leaks is lazy fields in data
structures.  There are some cases where lazy fields are required, but
in the cases that they are not required consider "making invalid
laziness unrepresentable" using strictness annotations and a strict
data types library like
[`strict`](https://hackage.haskell.org/package/strict) or
[`strict-wrapper`](https://hackage.haskell.org/package/strict-wrapper-0.0.0.0/docs/Data-Strict-Wrapper.html).
This practice might eliminate a substantial proportion of your thunk
leaks!

Although `strict-wrapper` provides a convenient strict version of
"small" types like tuples, eithers and maybes, it is not clear what
can be done for "large" data structures like lists and maps.
Converting the lazy form of a large data structure into the strict
form requires walking the whole structure, so one cannot do better
than a `deepseq`.  If one wants a spine strict alternative to a list
perhaps it is best just to directly use a
[`Seq`](https://hackage.haskell.org/package/containers-0.6.3.1/docs/Data-Sequence.html),
[`Vector`](https://hackage.haskell.org/package/vector-0.12.3.1/docs/Data-Vector.html)
or
[`Array`](https://hackage.haskell.org/package/array-0.5.4.0/docs/Data-Array.html),
and if one wants a spine and value strict alternative then perhaps
[`Vector.Unboxed`](https://hackage.haskell.org/package/vector-0.12.3.1/docs/Data-Vector-Unboxed.html)
or
[`UArray`](https://hackage.haskell.org/package/array-0.5.4.0/docs/Data-Array-Unboxed.html)
are the right choices.

# See also

* [Nested strict data in Haskell](../nested-strict-data/)

* [Haskell's `Data.Map.Strict.Map` is not a strict
  map](../data-map-strict-map-not-strict-map/)

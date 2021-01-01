# Why did it take four years to add two warnings to `-Wall`?

## Background

In August 2017 [I made a GHC proposal to add `incomplete-uni-patterns`
and
`incomplete-record-updates`](https://github.com/ghc-proposals/ghc-proposals/pull/71)
to `-Wall`.  The change implementing the proposal [was merged in
December
2020](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4614).  It
missed the 9.0 release window but will be present in 9.2.  According
to GHC's [release
conventions](https://gitlab.haskell.org/ghc/ghc/-/wikis/working-conventions/releases)
9.2 will be released in August 2021.

Why did it take four years to get these simple warnings into `-Wall`?
Adding them should have been easy, right?  Wrong.

## What do the warnings do

### `incomplete-uni-patterns`

Haskell allows you to write a pattern in the binder of a lambda
expression.  For example, you can define a function like

```haskell
f :: Maybe Int -> Int
f = \(Just n) -> n + 10
```

But this is a bad function to write because if you evaluate `f
Nothing` then it will fail at run time.

```
> f Nothing
*** Exception: Non-exhaustive patterns in lambda
```

Failures like this are easy to spot statically so we don't want them
in our Haskell programs.  Fortunately there is a GHC warning called
`incomplete-uni-patterns` that will spot this mistake and warn us.
Unfortunately it is not enabled by default nor in the grab bag of
useful warnings called "`-Wall`".  The latter is particularly
confusing since if I wrote `f` as

```haskell
f :: Maybe Int -> Int
f (Just n) = n + 10
```

then enabling `-Wall` *would* trigger a warning because
`incomplete-patterns` spots this particular case and it *is* in
`-Wall`.

### `incomplete-record-updates`

Haskell allows you to define record data types that have field names.
The field names can be used when creating records, extracting
components from records and updating records.  Unfortunately these
field names act very poorly with sum types.  For example, it is highly
dubious to define field names like

```haskell
data Foo = Bar { bar :: Int } | Quux { quux :: Bool }
```

Why is it dubious?  Because if, for example, you use `quux` in an
attempt to update a `Foo` which is actually a `Bar` then you will
receive a run time error.

```
> (Quux True) { bar = 1000 }
*** Exception: Non-exhaustive patterns in record update
```

The GHC warning `incomplete-record-updates` will warn us about this
but nor it is in `-Wall`.  Again this is confusing since if I try to
create a record with a missing field then I do get a
`no-missing-fields` warning at compile time.  Furthermore this warning
is a *default* warning.  I don't even have to enable `-Wall` to get
it!

## The proposal

Such beneficial changes should be quick to get past the GHC committee
right?  Wrong.

Haskellers generally appreciate the great static safety of the
language.  Even though these warnings exist they need to be somewhere
default for people to actually use them in practice.  `-Wall` should
really be "programming according to best practice".  It's not perfect,
but it's pretty good.

[My original suggestion on
Reddit](https://www.reddit.com/r/haskell/comments/6q9tcp/ghc_warnings_you_should_use_in_addition_to_wall/dkvrk0e/)
received 49 upvotes.  Buoyed by this enthusiasm I submitted at [GHC
proposal](https://github.com/ghc-proposals/ghc-proposals/pull/71)
where what seemed to me to be an obvious improvement received a
surprising amount of griping.  It seems that [the committee finally
accepted it six months
later](https://github.com/ghc-proposals/ghc-proposals/pull/71#issuecomment-364714258).

## The GHC process

I assumed that once the proposal was accepted a GHC developer would
change a couple of lines in the GHC codebase and the work would be
complete.  Should be easy right?  Wrong.

Someone [did create a ticket and start work on
it](https://gitlab.haskell.org/ghc/ghc/-/issues/15656) 13
months after the proposal was submitted.  The change to enable the
warnings themselves is simple

```patch
+        Opt_WarnIncompletePatternsRecUpd
+        Opt_WarnIncompleteUniPatterns
```

The difficulty comes from GHC's validation policy.  GHC requires that
its own source code must be clean for the warnings in `-Wall`.

[There were 126 files in GHC's source
code](https://gitlab.haskell.org/ghc/ghc/-/commit/4bada77d5882974514d85d4bd0fd4e1801dad755)
where the GHC developers had themselves used code that violated the
policy of `incomplete-uni-patterns` and `incomplete-record-updates`.


[The original ticket creator had already done most of this
work](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/181).  I
probably rebased his patch on the contemporary master although I'm not
completely certain.  Certainly it required some manual jiggery.


# Notes

* After year 1 I filed some tickets

  * <https://github.com/haskell/cabal/issues/6355>
  * <https://github.com/haskell/containers/issues/685>
      * Original developer did try to tackle this one
        <https://github.com/haskell/containers/issues/590>
  * <https://github.com/haskell/stm/issues/25>
  * <https://github.com/haskell/xhtml/issues/12>
      * This one seems not to be needed

  But what I *should* have done was ask them to enable
  `-Wincomplete-uni-patterns` and `-Wincomplete-record-updates` on
  their build because that would have pointed them to exactly where
  the problems lay.


* stm was not mirroring

> Pull mirroring failed 11 months ago.  Repository mirroring has been
> paused due to too many failed attempts, and can be resumed by a
> project maintainer.  Last successful update 11 months ago.

* I did not know that one *could* push to the GitLab mirrors.  I
  thought they were supposed to be exact duplicates.

* containers approved but did not merge
  * haddock has branches for each GHC version
  * I PRed to the wrong one
  * https://github.com/haskell/haddock/pull/1268
  * Then https://github.com/haskell/haddock/commit/acf235d607879eb9542127eb0ddb42a250b5b850
  * https://gitlab.haskell.org/ghc/ghc/-/issues/16642

* Haddock doesn't actually check that PR's build against ghc:master

https://github.com/haskell/haddock/pull/1268#issuecomment-745988796

* The only thing that will check whether I've done it right is running
  "validate" because that's the only thing that builds all the cabal
  packages with -Werror.  But by default ./validate cleans the source
  tree first and so one has to wait tens of minutes for the result

* Helpful people
  * RAE
  * Sylvain Henry
  * Alp
  * Ben Gamari
  * Sebastian Graf (check name is correct)

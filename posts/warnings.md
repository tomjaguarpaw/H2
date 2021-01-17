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

But this is a bad function to write because evaluating `f Nothing`
will fail with an error.

```
> f Nothing
*** Exception: Non-exhaustive patterns in lambda
```

Such problems are easy to spot statically so we would prefer it if the
compiler could tell us about them.  Fortunately there is a GHC warning
called `incomplete-uni-patterns` that will spot this mistake and warn
us.  Unfortunately it is not enabled by default nor in the grab bag of
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
field names interact poorly with sum types.  For example, it is highly
dubious to define field names like

```haskell
data Foo = Bar { bar :: Int } | Quux { quux :: Bool }
```

Why dubious?  Because if, for example, you use `quux` in an attempt to
update a `Foo` which is actually a `Bar` then you will receive a run
time error.

```
> (Quux True) { bar = 1000 }
*** Exception: Non-exhaustive patterns in record update
```

The GHC warning `incomplete-record-updates` will warn us about this
but it is not in `-Wall` either.  Again this is confusing since if I
try to create a record with a missing field then I do get a
`no-missing-fields` warning at compile time.  Furthermore this warning
is a *default* warning.  I don't even have to enable `-Wall` to get
it!

## The proposal

Having these warnings in `-Wall` seems beneficial. Such a change
should be quick to get past the GHC committee right?  Wrong.

Haskellers generally appreciate the great static safety of the
language.  Even though these warnings can be specifically enabled,
their full benefit won't be felt unless they are enabled through some
sort of default.  `-Wall` should really be "programming according to
best practice".  It's not perfect, but it's pretty good.

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

Someone [created a ticket and started work on
it](https://gitlab.haskell.org/ghc/ghc/-/issues/15656) 13 months after
the proposal was submitted.  The change to enable the warnings
themselves is simple

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
work](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/181).


## I took over the development

I probably rebased his patch on the contemporary master although I'm
not completely certain.  Certainly it required some manual jiggery.

I was flying completely blind.  I had no idea about how to develop on
GHC how to build it, how to submit merge requests.

There was no way that I was going to actually *fix* the causes of each
of these warnings.

I fixed all 126 locations that were raising those warnings by
suppressing the warning with a `OPTIONS_GHC` pragma at the top of the
file.

Once that was complete we're ready to merge, right?  Wrong.  GHC not
only requires that it be able to build itself clean for `-Wall` but
all the boot libraries too!  Boot libraries are the normal Haskell
packages that GHC itself depends on, like `cabal`, `stm` and
`containers`.

Now the problem becomes a lot more difficult.  Adding this warning to
GHC, and suppressing it in GHC's source, with a single commit is not
enough.  We also have to suppress it in the source of all boot
packages and make a single commit that updates the boot packages'
submodules at the same time.

That means I, or the boot package maintainers, have to go through the
source of all boot packages and make sure it is clean for `-Wall`.

I decided to get to a stable position of adding
`-Wincomplete-uni-patterns` and `-Wincomplete-record-updates` to the
build of GHC so no new such bad behaviour was added.  Even this was
non trivial.  There are two separate build systems (make and Hadrian)
and I needed two separate people (Ben Gamari and Alp Mestanogullari)
to tell me where exactly to find the relevant place to add the
warning.

I filed some tickets and gave up.

  * <https://github.com/haskell/cabal/issues/6355>
  * <https://github.com/haskell/containers/issues/685>
      * Original developer did try to tackle this one
        <https://github.com/haskell/containers/issues/590>
  * <https://github.com/haskell/stm/issues/25>
  * <https://github.com/haskell/xhtml/issues/12>
      * This one seems not to be needed

## Pushing things on

After a year I came back to it.

I had suppressed every raised warning in the boot packages during the
validation of GHC and forwarded the list of such warnings to the
maintainers of the boot packages.  However this proved not to be a
great strategy.  For one thing, [it was request which caused
puzzlement](https://github.com/haskell/cabal/issues/6355#issuecomment-554724281).
It's understandable.  Saying "please suppress this warnings that don't
occur" seems like a request to make.

But what I *should* have done was ask them to enable
`-Wincomplete-uni-patterns` and `-Wincomplete-record-updates` on their
build and then fix any errors that come up. That would have pointed
them to exactly where the problems lay and ensured that no new code
that triggers the warning would be added.

# Notes


* stm was not mirroring https://gitlab.haskell.org/ghc/packages/stm/-/issues/1

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

* [The Haddock policy is up for
  revision](https://hackmd.io/zXC78N8JTPi34BTXHBaFgg)

* 19:43  * bgamari_ wishes he understood how tomjaguarpaw is so good at finding all of these bugs

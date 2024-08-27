# Upgrading from GHC 8.10 to GHC 9.6

-- An experience report

At work ([Groq](https://groq.com/)) we recently upgraded the version
of GHC that we use from 8.10 to 9.6, along with many of the Haskell
packages we depend on.  Some of the changes to GHC and the packages we
depend on were "breaking changes", that is, changes which forced us to
change our own code in response.  This document details all such
changes that we had to make to our own code.  Hopefully it serves as a
measure of the effort required to keep up with breaking changes in the
Haskell ecosystem, encourages library and compiler maintainers to
avoid making breaking changes where reasonable, and where breaking
changes are made, to make them in a way that allows forward-compatible
mitigations rather than breaking fixes.

## Forward-compatible mitigations versus breaking fixes

I distinguish two kinds of behavior-preserving changes that are
required in response to an upgrade which contains breaking changes:
"forward-compatible mitigations" and "breaking fixes".
*Foward-compatible mitigations* are those changes that can be made to
the codebase before the upgrade such that no further changes need to
be made at the same time as the upgrade.  For example, suppose that
library "`mylibrary`" version 1 exports `functionOld`, which is
deprecated, and `functionNew` which is identical except it is not
deprecated. Further suppose that `mylibrary` version 2 removes
`functionOld`.  A forward-compatible mitigation would be to replace
uses of `functionOld` with `functionNew`.  Subsequently, the code
works equally well with `mylibrary` version 1 and with `mylibrary`
version 2, so no change needs to be made at the same time as the
upgrade of `mylibrary` from version 1 to version 2.

By contrast, *breaking fixes* are those changes that only work after
an upgrade.  For example, suppose that library "`anotherlibrary`"
version 1 exports `anotherFunction :: A -> B -> C`, and
`anotherlibrary` version 2 exports `anotherFunction :: B -> A -> C`,
which is identical except that the argument order is switched.  A
breaking fix would be to replace all uses of `anotherFunction` by
`flip anotherFunction`.  Breaking fixes complicate an upgrade because
they can't be made before or after the upgrade but must be made at
exactly the same time.

### Turning breaking fixes into forward-compatible mitigations

Every breaking fix can be made forward-compatible by wrapping it in
CPP, but that introduces its own complexity, not least because CPP
only works on a per-line basis rather than a per-expression basis. We
didn't use CPP in our upgrade.  Another technique for making some
breaking fixes into forward-compatible mitigations is the
"compatibility shim".  For example, in the case of `anotherlibrary`
above, the author could have added `anotherFunctionNew :: B -> A -> C`
to version 1 and retained it in version 2, allowing users to make the
forward-compatible mitigation of switching from `anotherFunction` to
`anotherFunctionNew` before the upgrade.  At their leisure, any time
after the upgrade, they could switch to `flip anotherFunction`.
Critically, they are not forced to make the switch *at the same time
as the upgrade*[^1].

## The changes

### Changes to libraries

#### New export from `Prelude`

Newer versions of `base` re-export `liftA2` from the `Prelude`
therefore explicit imports from `Control.Applicative`emit warnings.
The *forward-compatible mitigation* is to turn on
`-Wwarn=unused-imports` but we chose the *breaking fix* of removing
the explicit import.

#### `ST` instance of `MonadFail` removed

The `instance MonadFail (ST s)` was removed in a recent `base`
version.  The *forward-compatible mitigation* is to use `error`
instead of `fail` when in `ST`.

#### Removed exports from `mtl` modules

Some `mtl` modules, for example `Control.Monad.Except`, previously
re-exported transformer-related functionality such as `lift` and
`MonadTrans`.  More recent `mtl` versions do not so the imports must
come from elsewhere, such as `Control.Monad.Trans`.  This is a
*forward-compatible mitigation*.

#### `aeson` API change

Version two of `aeson` changed its representation of JSON objects from
`HashMap Text Value` to `KeyMap Value` because the former is
vulnerable to denial-of-service attacks.  The *breaking fix* is to use
the new API, and conversion functions where necessary, for example
`Data.Aeson.KeyMap.lookup` instead of `Data.HashMap.Strict.lookup`,
and `fromText` and `toText` to convert between `Text` and `Key`.

Parts of the breaking fix can be made *forward-compatible* by clever
use of imports and local definitions as a sort of compatibility shim.
For example, if you write this code, which works under `aeson-1`:

```.hs
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Value)
import Data.HashMap.Strict qualified as Aeson

type Key = Text
type KeyMap = Aeson.HashMap Key

fromText :: Text -> Key
fromText = id

lookupColor :: KeyMap -> Value
lookupColor m = Aeson.lookup (fromText "color")

... more code
```

then converting it to `aeson-2` only requires tweaking imports, not
the bulk of the code:

```.hs
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Value)
import Data.Aeson.Key (Key, fromText)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as Aeson

lookupColor :: KeyMap -> Value
lookupColor m = Aeson.lookup (fromText "color")

... more code (unchanged)
```

A team could reduce the churn even further by creating a compatibility
module in their codebase, `Data.Aeson.Compat` say, which contains
those definitions. Then the client code would look like the following
and not need to change at all when switching to `aeson-2`!  Only
`Data.Aeson.Compat` would need to change.

```.hs
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Value)
import Data.Aeson.Compat qualified as Aeson
import Data.Aeson.Compat (Key, KeyMap)

lookupColor :: KeyMap -> Value
lookupColor m = Aeson.lookup (fromText "color")

...
```

In fact, this kind of compatibility module could have been placed in
an `aeson-compat` package on Hackage for everyone to use, or even
officially included in `aeson` itself.

#### `I64#` change

The [definition of `Int64` used to
be](https://hackage.haskell.org/package/base-4.14.3.0/docs/GHC-Int.html#t:Int64)
`data Int64 = I64# Int#` but more recently [it
became](https://hackage.haskell.org/package/base-4.20.0.0/docs/GHC-Int.html#t:Int64)
`data Int64 = I64# Int64#`.  This requires a *breaking fix*.  In our
case we shouldn't have been using this low-level representation in the
first place, and it turned out the code that was using it was not
itself used, so we deleted it (which luckily for us is a
*forward-compatible mitigation*).

#### `xls` API change

The `xls` library introduced `RowIndex` and `ColumnIndex` abstract
types where it previously used `Int`.  As with `aeson` this required a
*breaking fix* which could be implemented partially as a
*forward-compatible mitigation* by defining type synonyms locally as a
sort of compatibility shim.  The upgrade would have been easier if the
library itself had introduced those synonyms before making the
breaking change.


The library also added `wsState` as a field to its `Worksheet` type,
which required a *breaking fix* when constructing values of that type.

#### `flatparse` API change

[`anyCharASCII`](https://hackage.haskell.org/package/flatparse-0.3.5.1/docs/FlatParse-Basic.html#v:anyCharASCII)
became
[`anyAsciiChar`](https://hackage.haskell.org/package/flatparse-0.5.1.0/docs/FlatParse-Basic.html#v:anyAsciiChar),
along with many similar API changes.  If we had a large amount of
`flatparse` code we might have used the compatibility module approach
in order to make a partially *forward-compatible mitigation* but
instead we made the *breaking fix*.

#### `constraints-extras` API change

In [previous versions of
`constraints-extras`](https://hackage.haskell.org/package/constraints-extras-0.3.0.2/docs/Data-Constraint-Extras.html#t:Has)
`Has` was a type synonym whereas in later versions [it became a type
class](https://hackage.haskell.org/package/constraints-extras-0.4.0.0/docs/Data-Constraint-Extras.html#t:Has).
This required a *breaking fix*.


#### `genSingletons`

We couldn't get
[`genSingletons`](https://hackage.haskell.org/package/singletons-2.6/docs/Data-Singletons-TH.html#v:genSingletons),
from the `singletons` family of packages, to work after the upgrade.
The *breaking fix* we applied was to write out by hand what would have
been generated by Template Haskell.

### Changes to GHC

#### Simplified subsumption

GHC 9.0 made a switch to a type inference scheme for higher rank
polymorphism called [simplified
subsumption](https://github.com/ghc-proposals/ghc-proposals/pull/287).
Consquently, expressions like `g f` in the below, which passed the
type checker in the GHC 8 series, stopped passing the type checker in
the GHC 9 series.


```.hs
{-# LANGUAGE RankNTypes #-}

f :: Int -> (forall a. a -> a)
f _ = id

g :: (forall a. Int -> a -> a) -> ()
g _ = ()

main :: IO ()
main = print (g f)
```

The *forward-compatible mitigation* is to manually eta-expand `f` to
obtain `g (\x -> f x)`, as in the below.

```.hs
main = print (g (\x -> f x))
```

The *breaking fix* is to disable simplified subsumption by using the
`DeepSubsumption` language pragma (available in GHC 9.2 and later).

#### GHC bug

We came across a bug where GHC errored out with "The impossible
happened" in some code related to existential types.  We had to pull
an inner binding to the top-level and mark it `NOINLINE` to avoid
triggering the bug.  This was a *forward-compatible mitigation*.


#### Overlapping patterns

Improvements to GHC's pattern match checker mean that some patterns
are now detected as redundant where they weren't previously.  The
*forward-compatible mitigation* of using `-Wwarn=overlapping-patterns`
has too big a downside so we used the *breaking fix* of removing the
redundant patterns.

#### Kind `*` becomes `Type`

The GHC 9 series introduces a warning in `-Wall` about using `*` as
the kind of types.  The *forward-compatible mitigation* is to replace
`*` with `Data.Kind.Type`.

#### GADT mono local binds

The GHC 9 series introduces a warning in `-Wall` about pattern
matching on a GADT without `MonoLocalBinds`.  The *forward-compatible*
mitigation is to enable `MonoLocalBinds`. Alternatively there is a
*breaking fix* which is to disable the `gadt-mono-local-binds`
warning.

### Changes to tools

#### `brittany` is no longer maintained

The formatter `brittany` is no longer maintained.  We applied the
*forward-compatible mitigation* of no longer using `brittany` to
format a subset of our code.

#### Hadrian

Hadrian, the (relatively) new GHC build system, refuses to build
`ghci` when cross-compilation is enabled.  We applied the
*forward-compatible* mitigation of no longer building cross-compiled
GHC.

[^1]: [The Opaleye API breakage
policy](http://h2.jaguarpaw.co.uk/posts/opaleyes-api-breakage-policy/)
is upheld by the heavy use of compatibility shims.

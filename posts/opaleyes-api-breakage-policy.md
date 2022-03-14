# Opaleye's API breakage policy

-- by [Tom Ellis](http://web.jaguarpaw.co.uk/~tom/contact/)

## Unpublished.  Do not circulate.

This article is not ready for general circulation.  Please share your
thoughts with the author but do not submit to Reddit, Twitter, etc..

---

## Introduction

### Opaleye and API breakage

I am the maintainer of
[Opaleye](https://hackage.haskell.org/package/opaleye), a Haskell
package for writing queries against PostgreSQL.  Occasionally I "break
the API" of Opaleye, that is, I release a new version of the package
such that some code written against the previous version of the
package won't work with the new version[^break].  Such breakage
imposes a cost on Opaleye's users: it takes effort to change code to
make it work with the new version.  There is a related cost which is
often left unaccounted for: it takes effort even to discover what
changes need to be made!

In my experience of being on the receiving end of API breakage the
latter cost is actually the more severe.  Once I know what change
needs to be made I can come up with a plan and accurately assess the
effort involved.  On the other hand, if the only information that I
receive is a compile error, and no breadcrumbs pointing me in the
direction of a solution, then I am left staring at a long-tailed
distribution: for all I know fixing the breakage could take me a
minute, an hour or a day.  Under such uncertainty I am disinclined to
even start.

### Why break?

The aim of Opaleye is to be of service to users, so if API breakage
imposes a cost on users then why break the API at all?  Firstly, as
sole maintainer I need to keep my burden light, and striving for a
small API surface area is one way of doing that.  Secondly, our
understanding of Opaleye best practice evolves over time, and I want
to nudge users towards best practice.  Thirdly, I want the API to be
approachable, and removing old, deprecated parts of the API helps that
end.

This rationale is the justification for occasional API breakage.  The
alternative is to keep old, discouraged parts of the API around, but
to warn against them, deprioritise them in documentation and avoid
mentioning them in polite conversation.  That alternative is a valid
approach too, perhaps even a desirable approach if one has the
resources to deal with the mental overhead, but I don't feel that I
do.

So, the aim of the Opaleye breakage policy is to strike a balance
between keeping the API small, simple, easy to understand and
maintainable on the one hand, and reducing the costs of churn on the
user on the other.  Later versions will inevitably become incompatible
with user code written against older versions, but there is much that
can be done to mitigate the costs on the user, to "hold the user's
hand" through the process, and make their life easier, some of which
the rest of this article will describe.

## Avoid flag day

The single biggest breakage cost that an API can impose is the need
for a "[flag
day](https://en.wikipedia.org/wiki/Flag_day_(computing))", that is "a
change which requires a complete conversion of a sizable body of
software".  The cost of a flag day is not just borne by the users of
the API individually; the changes that lead to flag days make software
ecosystems rigid and inflexible and impose coordination costs across
*the entire ecosystem*!  Flag days force the entire ecosystem to
change in lock step.

I work very hard to avoid "flag days" caused by Opaleye API changes.
Specifically, if a user has written code against Opaleye major version
`N`[^two-components] that is broken by Opaleye `N+1`, it should be
possible to write different code *with the same functionality* that
works against versions `N` and `N+1` at the same time.  For example,
when a function present in version `N` is removed in version `N+1`
there should always be another way of achieving the same functionality
that works across `N` and `N+1`.

## Concrete examples of API breakage

The following reasons for API breakage, with examples, elaborate on
the goal of keeping the API small, manageable and in line with best
practice.

* *Hiding exposed internals:* If part of the public API exposes
  internal details then one should either make those private details
  officially public or remove the part that exposed them.  The
  latter is often the judicious choice if one wants to preserve the
  simplicity of the API.

  For example,
  [`formatAndShowSQL`](https://hackage.haskell.org/package/opaleye-0.6.0.0/docs/Opaleye-Sql.html#v:formatAndShowSQL)
  exposed three internal types! It was a mistake to add it to a
  public module in the first place.

* *Replacing complicated functions with simple ones:* If there is a
  simpler way to achieve the same functionality it can reduce user confusion
  and ease maintenance burden to remove the more complicated version.

  For example, the type of
  [`leftJoinInferrable`](https://hackage.haskell.org/package/opaleye-0.6.7004.2/docs/Opaleye-Join.html#v:leftJoinInferrable)
  was designed so that the return type could be inferred from the
  argument types.  Shane O'Brien and Olly Charles from CircuitHub
  discovered much better ways, called
  [`optionalRestrict`](https://hackage.haskell.org/package/opaleye-0.6.7005.0/docs/Opaleye-Join.html#v:optionalRestrict)
  and
  [`optional`](https://hackage.haskell.org/package/opaleye-0.6.7005.0/docs/Opaleye-Join.html#v:optional).

* *Replacing special purpose functions with general ones:* Removing
  special purpose functions when more general ones exist (that are
  still simple) is a good way to keep the API small, reducing user
  confusion and to easing maintenance burden.

  For example,
  [`runInsert`](https://hackage.haskell.org/package/opaleye-0.6.0.0/docs/Opaleye-Manipulation.html#v:runInsert)
  (which could only insert a single element) was replaced with
  [`runInsertMany`](https://hackage.haskell.org/package/opaleye-0.7.0.0/docs/Opaleye-Manipulation.html#v:runInsertMany)
  (which could insert a list of elements).  But `runInsertMany` was
  subsequently found to be insufficiently general (because it did
  not support `RETURNING` or `ON CONFLICT`) and was replaced by
  [`runInsert_`](https://hackage.haskell.org/package/opaleye-0.7.0.0/docs/Opaleye-Manipulation.html#v:runInsert_)
  (which does).  Then, because the name `runInsert` had been freed
  up, `runInsert_` could be renamed `runInsert` (removing the `_`
  which was a rough edge in the API).


* *Renaming a misnamed function:* To make an API more understandable
  it can sometimes be useful to choose a better name for a
  badly-named function.

  For example, there was a function called
  [`optional`](https://hackage.haskell.org/package/opaleye-0.7.0.0/docs/Opaleye-Table.html#v:optional). Now,
  `optional` is a very valuable part of the namespace!  Not only is it
  a common English word, it is [used in
  `base`](https://www.stackage.org/haddock/lts-18.27/base-4.14.3.0/Control-Applicative.html#v:optional) and
  it is also used as the name of [a *different*, and more important,
  Opaleye
  function](https://hackage.haskell.org/package/opaleye-0.7.0.0/docs/Opaleye-Join.html#v:optional)
  (with similar functionality to the `base` one)!  Therefore I renamed
  the former `optional` to `optionalTableField`.

## How to hold the user's hand

Opaleye's *fundamental principle* of API breakage is that the user's
code should not break when upgrading to Opaleye major version `N+2`
unless

1. the documentation of (the last point release of) version `N` warned
of the impending breakage, and
2. the user received a deprecation warning when compiling against
`N+1`

Both the documentation warning and the formal deprecation should tell
the user what she needs to do to repair the breakage.  This way she
has plenty of advanced notice that she will need to take action, and
the action to take is clear.  She doesn't risk being thrown into a
surprise problem solving session without any idea of how much effort
she will have to spend.

### Deprecation cycle for renaming a function

The fundamental principle suggests, for example, that later versions
should never change the behaviour of a function.  Instead they should
add a new function with new behaviour, subsequently deprecate the old
function, and subsequently remove it.

For example, if my package contains a function `myFunction` but I
decide that `betterFunction` provides a better way of doing the job
that `myFunction` does, but for the reasons explained above it's
actually judicious to *remove* `myFunction` then this is how I would
do so.  Suppose the latest major version is `N`, then

1. In version `N` I would add `betterFunction`, and add a note to the
   Haddock documentation of `myFunction` to say that users should use
   `betterFunction` instead, and `myFunction` will be deprecated in
   version `N+1`

2. In version `N+1` I would deprecate `myFunction` and change the
   note to say that it will be *removed* in version `N+2`.

3. In version `N+2` I would remove `myFunction`.

#### Real world example

When
[`optionalRestrict`](https://hackage.haskell.org/package/opaleye-0.6.7005.0/docs/Opaleye-Join.html#v:optionalRestrict)
was discovered I realised that its API was much nicer than that of
[`leftJoinInferrable`](https://hackage.haskell.org/package/opaleye-0.6.7004.2/docs/Opaleye-Join.html#v:leftJoinInferrable),
so I wanted to promote the former and remove the latter.  Here's how
that worked in practice, following the recipe above.

* [`0.6.7004.2`](https://hackage.haskell.org/package/opaleye-0.6.7004.2/docs/Opaleye-Join.html#v:leftJoinInferrable)
  was the last version of Opaleye before `optionalRestrict` was added.

* [`0.6.7005.0`](https://hackage.haskell.org/package/opaleye-0.6.7005.0/docs/Opaleye-Join.html#v:leftJoinInferrable)
  (August 2020) introduced `optionalRestrict` and added to the Haddock
  of `leftJoinInferrable` the text *"Do not use. Will be deprecated in
  0.7. Use `optionalRestrict` instead."*

* [`0.7.0.0`](https://hackage.haskell.org/package/opaleye-0.7.0.0/docs/Opaleye-Join.html#v:leftJoinInferrable)
  (August 2020) formally deprecated `leftJoinInferrable` using `{-#
  DEPRECATED ... #-}`.  It still had the recommendation to use
  `optionalRestrict` (but I forgot to say that "it will be removed in
  0.8"!)

* [`0.8.0.0`](https://hackage.haskell.org/package/opaleye-0.8.0.0/docs/Opaleye-Join.html)
  (November 2021) removed `leftJoinInferrable`

### Deprecation cycle for removing a function

Removing a function follows the same deprecation cycle as renaming,
except that the package is not providing a direct functional
replacement.  Because there is no direct replacement it's valuable to
leave a hint to users about how they should obtain similar
functionality.

### Deprecation cycle for modules, types and classes

#### Modules

The same deprecation cycle works for removing and renaming modules.
For example, the module
[`Opaleye.Constant`](https://hackage.haskell.org/package/opaleye-0.6.7006.1/docs/Opaleye-Constant.html)
was renamed
[`Opaleye.ToFields`](https://hackage.haskell.org/package/opaleye-0.6.7006.1/docs/Opaleye-ToFields.html)
by, first, in version 0.6, [leaving a
note](https://hackage.haskell.org/package/opaleye-0.6.7006.1/docs/Opaleye-Constant.html)
to use the new name and that the old name would be deprecated, then
second, in version 0.7, [deprecating the old
module](https://hackage.haskell.org/package/opaleye-0.7.0.0/docs/Opaleye-Constant.html),
and third, in version 0.8, removing the old module.

#### Types

One can use a similar deprecation cycle to rename types, by taking
advantage of type synonyms.  For example the type
[`Opaleye.Constant.Constant`](https://hackage.haskell.org/package/opaleye-0.6.7006.1/docs/Opaleye-Constant.html#t:Constant)
was renamed
[`Opaleye.ToFields.ToFields`](https://hackage.haskell.org/package/opaleye-0.6.7006.1/docs/Opaleye-Constant.html#t:ToFields).
First, in version 0.6, [I left a
note](https://hackage.haskell.org/package/opaleye-0.6.7006.1/docs/Opaleye-Constant.html#t:Constant)
to use the new type synonym, and that the old name would be deprecated
in version 0.7.  At this stage the new name was a [type synonym for
the old
name](https://hackage.haskell.org/package/opaleye-0.6.7006.1/docs/Opaleye-Constant.html#t:ToFields).
Second, in version 0.7, I switched things around so that [the *old*
name was a deprecated type synonym for the new
name](https://hackage.haskell.org/package/opaleye-0.7.0.0/docs/Opaleye-Internal-Constant.html#t:Constant),
with a note that it would be removed in 0.8.  Thirdly, in version 0.8
the old name was removed[^2].


#### Classes

Because types and classes are treated in a very similar way by the
type system the same deprecation cycle for types works for classes
too, but with a rough edge.  One cannot use a `type` synonym to name a
class in an instance declaration for that class.  For example, to
rename
[`QueryRunnerColumnDefault`](https://hackage.haskell.org/package/opaleye-0.6.7006.1/docs/Opaleye-Internal-RunQuery.html#t:QueryRunnerColumnDefault)
to
[`DefaultFromField`](https://hackage.haskell.org/package/opaleye-0.6.7006.1/docs/Opaleye-Internal-RunQuery.html#t:DefaultFromField)
I used the same technique above of defining a type synonym

```haskell
type DefaultFromField = QueryRunnerColumnDefault
```

The synonym could be used in most places that original class name
could be used, but *not* in instance heads.  For example, I can write
the following, but I cannot replace `QueryRunnerColumnDefault` in the
instance head with `DefaultFromField`.

```haskell
instance DefaultFromField a b
   => QueryRunnerColumnDefault (Nullable a) (Maybe b) where
   ...
```

Perhaps this is a weakness in the language.  It certainly prevents a
smooth deprecation cycle for class names.

## Weaknesses

It's not always possible to avoid flag day and it's not always
possible to uphold the "fundamental principle".  For example, the type
class synonym example above shows that it is impossible to avoid a
flag day when renaming classes.  In version `N` users would *have to*
use `QueryRunnerColumnDefault` in type class instance heads, and in
version `N+1` they would *have to* use `DefaultFromField`.
Regrettably there is no way to write code that works across version
`N` and `N+1`[^cpp].  Perhaps a change to GHC could add support for
this workflow.

Another example is a change to Opaleye's type of SQL fields or
columns, that used to be called `Column`.  Non-nullable columns were
represented as, for example, `Column SqlText` and nullable columns
were represented as `Column (Nullable Sql)`.  Unfortunately the
nonsensical `Column (Nullable (Nullable SqlText))` was not prevented.
To tighten up the types, version 0.9 improved the situation by
replacing `Column` with `Field_`[^field], which has the distinction
`Field_ NonNullable SqlText` vs. `Field_ Nullable SqlText`.  Replacing
`Column` with `Field_` caused a flag day for the Opaleye ecosystem.
Although there is [a compatibility type
family](https://hackage.haskell.org/package/opaleye-0.9.2.0/docs/Opaleye-Column.html#t:Column)
it's not enough to guarantee that user code can work across 0.8 and
0.9 at once.

## Summary

There is a wide spectrum between "never break userspace" and "move
fast and break things"[^slogans] and the right place on that spectrum
for a project to position itself can only be determined on a
case-by-case basis by taking into account the needs of the project
maintainers and users.  If the project decides that some rate of API
breakage is desirable then the maintainers can make life easier for
the users by following a principle like Opaleye's fundamental
principle if API breakage.  Users' lives are made easier if they are
given breadcrumbs and hand-holding when their code breaks.

Do you have thoughts or comments on this article?  Do you know
articles explaining similar API breakage policies?  Or different
policies?  Would you like my help designing a deprecation cycle for
your library?  Feel free to [contact
me](http://web.jaguarpaw.co.uk/~tom/contact/).


## References

This article was partly inspired by Chris Done's [Immutable
Packaging Policy](https://chrisdone.com/posts/ipp/).


<!-- --- -->

<!-- --- -->

<!-- # Scratch notes -->

<!-- ## Principles in play -->

<!-- * Knowledge of the best way to present an API evolves over time. -->

<!-- * Removing parts of APIs that, with the benefit of hindsight, are -->
<!--   poorly designed or have better replacements, lowers future -->
<!--   maintenance overhead and increases future user accessibility. -->

<!-- * Current users benefit from clear and timely warnings of API -->
<!--   breakage and from a smooth and unhurried upgrade path. -->

<!-- * Current users benefit from clear explanations about how to adapt -->
<!--   their code to the new API version. -->

<!-- * The ecosystem is more resilient when whole ecosystem is not forced -->
<!--   to upgrade at once (that is, when it avoids "[flag -->
<!--   days](https://en.wikipedia.org/wiki/Flag_day_(computing))") -->

<!-- -- -->

[^break]: Naturally when I do so I change the version number according to
    the [Package Versioning Policy](https://pvp.haskell.org/).


[^two-components]: For a Haskell package following the [Package
    Versioning Policy](https://pvp.haskell.org/) (PVP), the major
    version `N` will actually be formed of two numeric components
    `A.B`.

[^2]: from the public API.  Technically it's still around in an
    `Internal` module.

[^cpp]: short of CPP

[^field]: `Field_` has an underscore in its name to leave the name
    `Field` for the type synonym of `Field_ NonNullable`

[^slogans]: Slogans due to Linus Torvalds, founder of the Linux
    kernel, and Mark Zuckerberg, founder of Facebook.

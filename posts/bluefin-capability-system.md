# Bluefin is a capability system

-- Tom Ellis, September 2026

[Bluefin](https://hackage.haskell.org/package/bluefin) is what the
Haskell world calls an "effect system".  I released [the first
version](https://hackage.haskell.org/package/bluefin-0.0.0.0) in early
2024, basing it closely on
[`effectful`](https://hackage.haskell.org/package/effectful) by
Andrzej Rybczak.  Similarly to `effectful`, Bluefin is an
"`IO`-wrapper" (or "analytic") effect system, meaning that it is a
lightweight wrapper around Haskell's `IO` type[^1].  The major
difference between `effectful` and Bluefin is that in `effectful` the
presence of effects is indicated by constraints in the type system (a
familiar and traditional approach) whereas in Bluefin the presence of
effects is indicated by value level arguments.

There is a notion going back several decades in the computer science
literature called "capability system". Now, in late 2026, I have
decided that I will start referring to Bluefin as a "capability
system" first and an "effect system" second, that is, something like
"a capability system that can be used to obtain the benefits of a
traditional Haskell effect system".  Why?  Well, let's have a look at
what a capability system is, scrutinise more closely the notion of
effect system, and see what best describes Bluefin.

## Capability systems

A *capability system* is a programming language framework for working
with "capabilities", a *capability* being a program entity that
references a program or system resource and, moreover, grants
authority to access that resource.

> In a capability system, each capability points from a subject to a
> resource. Consequently every capability can serve both to designate
> which resource to access, and to provide the authority to perform
> that access.

-- [Capability Myths Demolished](http://zesty.ca/capmyths/); Miller,
Yee, Shapiro

A capability that most programmers will have encountered is the file
handle.  In the words of Lampson[^3]:

> A familiar example in operating systems is a file descriptor or file
> handle for an open file. When a process opens the file, the OS
> [after checking that the process has sufficient rights to access the
> file] creates a handle for the open file.

The notion of capability was originally introduced for security
purposes, as a model for access control and protection in operating
systems[^4].  An example of a capability-based kernel is
[seL4](https://en.wikipedia.org/wiki/SeL4) (interestingly, it has an
executable specification written in Haskell!).  In se4L a process may
have the capability to send a message to a "mailbox" (called an
"endpoint") and another process may have a capability to *receive*
messages from the same mailbox.  This allows the inter-process
communication (IPC) between the two processes. (See the [se4L
FAQ](https://sel4.systems/About/FAQ.html#how-does-message-passing-work)
for more details.)

Capabilities are also used in programming languages
(e.g. [Pony](https://en.wikipedia.org/wiki/Pony_(programming_language))),
where they are often referred to as "object capabilities", and
authorize access to entities in the language's runtime system (RTS)
rather than entities in the operating system (OS), although since the
RTS typically provides access to the OS the boundary is blurry.  An
example of a capability in Pony would be a `StartProcessAuth`, the
capability to launch a new process.  A Pony program can call the
`StartProcess` function to launch a new process but only if it has a
`StartProcessAuth` capability that it can pass to that call.  See the
[Pony Process package
documentation](https://stdlib.ponylang.io/process-StartProcess/) for
more information.

After writing Bluefin I realised that there's no reason to view
capabilities in a narrow, security-only context.  Effect systems can
be considered to be capability systems too.  But to understand what
that means we have to look more closely at what we mean by "effect
system".

## Effect systems

### Definition

In Haskell the notion of "effect system" is not precisely defined.
However, all usages of the terminology have something important in
common: an effect system allows the programmer to circumscribe the
range of externally visible behaviours of program components.

Interpreted narrowly, "effect system" might mean a library in the
style of
[`extensible-effects`](https://hackage.haskell.org/package/extensible-effects/)[^5],
in which all effects take place in a specific monad (e.g. in
`effectful`
"[`Eff`](https://hackage-content.haskell.org/package/effectful-core-2.7.1.1/docs/Effectful.html#t:Eff)")
and the possibility of an effect is indicated by a constraint, i.e. at
the type level. For example, the behaviour of the following function
(given using types from `effectful`) is circumscribed such that it may
only interact with a database and/or throw an exception of `String`:

```.hs
operationEffectful ::
  (Database :> es, Error String :> es) =>
  Eff es ()
```

Bluefin is an effect system, if we permit a slight broadening of this
narrow interpretation.  In Bluefin, the difference is that the
possibility of an effect is indicated by a value-level argument, for
example:

```.hs
operationBluefin ::
  Database es ->
  Throw String es ->
  Eff es ()
```

Widening the definition slightly further allows
[MTL](https://hackage.haskell.org/package/mtl) to be considered an
effect system:

```.hs
operationMTL ::
  (MonadDatabase m, MonadError String m) =>
  m ()
```

or even
[`transformers`](https://hackage.haskell.org/package/transformers):

```.hs
operationTransformers ::
  DatabaseT (Except String) ()
```

A very broad definition allows
[`ST`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Monad-ST.html)
to be considered an effect system (that only allows mutable state
effects) and even `IO` (which allows mutable state, exceptions, and
the whole kitchen sink of interactions with the RTS).

(The order in which the effect systems were presented here is close to
the *opposite* of the order in which they were developed historically.
For more information see my talk "[A History of Effect
Systems](https://www.youtube.com/watch?v=RsTuy1jXQ6Y)".)

### Absence of effects

It is important to emphasise that the reason effect systems are useful
is not that they allow the programmer to define program components
that perform effects. Rather, effect systems are useful because they
allow the programmer to define program components that are *forbidden*
from performing effects outside a specified set.

In the extreme we can forbid *all* externally visible effects, thereby
using the type system to enforce that our operation must be what is
sometimes called a "pure function" or a "mathematical function".  The
types for an operation that takes a `String` and returns an `Int`,
performing no externally visible effects, might look as follows in
different effect systems:

```.hs
computeEffectful :: String -> Eff es Int
computeBluefin :: String -> Eff es Int
computeMTL :: Monad m => String -> m Int
computeTransformers :: String -> Identity Int
```

In `ST` and `IO` there is no way of circumscribing the externally
visible effects short of not using them at all, so we could settle for
the following (which has no effect system in sight):

```.hs
computeFunction :: String -> Int
```

(It could be argued that such operations *do* have externally-visible
effects, such as taking time to run and performing allocations.  This
suggests that it is rather hard to define "effect" in a useful way.)

## Capability systems versus effect systems

In a capability system there are operations you can only perform if
you have access to a capability that provides authority to perform
them.  [Above](#capability-systems) we saw the example of a file
handle: you can only write to an open file if you have access to its
handle.  Correspondingly, in an effect system there are effectful
operations you can only perform if their corresponding effect is
somehow "in scope".  For example, you can only throw an exception if
some sort of error, exception or throw effect is in scope. (In
`extensible-effects` and its descendants, and in MTL, "in scope" would
mean "at the type level"; in Bluefin "in scope" would mean "at the
value level"; in `transformers` "in scope" would mean "carried by a
monad transformer".)  What, then, is the *difference* between
capability systems and effect systems?  I think there is no
difference!

Let's investigate the potential correspondence a bit more closely.
Can all capabilities be treated as effects?  When defining capability
systems above we noted that authority to access a file is provided by
a capability, often called a "file descriptor" or "file handle".
Similarly, if we held "the authority to access a database" that would
be considered a capability. Such a capability is generally provided
through a "connection handle".  The [`operation*` examples
above](#definition) show how different Haskell effect systems
encode such a capability.  Haskell effect systems already have the
ability to pass around, implicitly or explicitly, a program resource
that is required to use a particular program component.

How about the converse? Can all effects be treated as capabilities?  I
think yes. In order to perform a particular effectful operation
something has to be "in scope".  Whatever that is can be interpreted
as *the capability to perform that effect*.

### Effects as capabilities

Let's look at a few examples of effects interpreted in terms of
capabilities, firstly state. A state effect can be interpreted as a
capability that grants authority to interact with a mutable value of a
given type.  I think it is particularly interesting to look at the
case of `ST`, `IO` and `Bluefin`, where
[`STRef`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-STRef.html),
[`IORef`](https://www.stackage.org/haddock/lts-24.56/base-4.20.2.0/Data-IORef.html)
and
[`Modify`](https://hackage-content.haskell.org/package/bluefin-0.7.0.1/docs/Bluefin-Capability-Modify.html)
respectively provide what is traditionally called a "mutable
reference", at the value level.  A "mutable reference" matches exactly
the definition of capability we saw earlier ("a program entity that
references a program or system resource and, moreover, grants
authority to access that resource").  A mutable reference is a pointer
which references some mutable region of memory and "grants access to
it", i.e. if you hold such a mutable reference you can use it to
modify the mutable region of memory.

Secondly, let's look at exceptions.  An exception effect can be
interpreted as the capability which grants the authority to throw a
value of a particular type to a particular handler.  Again, I find the
case of `Bluefin` particularly interesting: a value of type
[`Throw`](https://hackage-content.haskell.org/package/bluefin-0.7.0.1/docs/Bluefin-Capability-Throw.html)
grants access to the exception mechanism, allowing you to throw an
exception to the handler that was installed by
[`try`](https://hackage-content.haskell.org/package/bluefin-0.7.0.1/docs/Bluefin-Capability-Throw.html#v:try)
(or `catch` or `handle`), the same site that introduced the `Throw`
into scope.

Here's an example.  Note that the `String`-typed exception is thrown
using the capability "`th`", and when that exception is thrown it is
handled at the `try` that brought `th` into scope (and cannot be seen
by any other `try` block)[^6].

```.hs
f n = try $ \th ->
  if n > 5
    then throw th "Too big"
    else pure (2 * n)

> runPureEff (f 4)
Right 8
> runPureEff (f 10)
Left "Too big"
```

Thirdly, a reader effect can be interpreted as the capability to
interact with some thread-local, locally-mutable state.  No such
primitive mutable state reference exists in Haskell at present, but
there is [a proposal to add
one](https://github.com/ghc-proposals/ghc-proposals/pull/751).  (I
like the name `IOScopedRef` for the new reference type[^7].  Bluefin
and `effectful` simulate this kind of reference, as
[`Ask`](https://hackage-content.haskell.org/package/bluefin-0.7.0.1/docs/Bluefin-Capability-Ask.html)
and `Reader` respectively.)

Finally, "I/O effects", i.e. the ability to interact in arbitrary ways
with the RTS, can be interpreted as the capability granting authority
to do so.  That is to say, `effectful`'s type level `IOE`, Bluefin's
value level `IOE`, MTL's `MonadIO` constraint and the plain `IO` monad
itself can all be interpreted as capabilities granting authority to
interact with the RTS.

## "Bluefin is a capability system"

So why have I decided to describe Bluefin as a "capability system"?
Because I think that "capability" terminology communicates better than
"effect" terminology.  Firstly, the latter raises the awkward question
of what an "effect" *is*.  One might attempt to define "effect" in
terms of opposition to a notion of "pure", but I've never seen a
convincing such attempt.  In fact I haven't come across a convincing
definition of "pure" either[^8].

Secondly, I think that, regrettably, the terminology "effect system"
gives the wrong impression about Haskell to outsiders, specifically
that, unlike other languages, Haskell needs a "special system" to do
"effects".  The converse is true: other languages, unlike Haskell,
cannot *circumscribe the possible range of effects*.  I hope that
using the terminology "capability system" to describe Bluefin moves
the perception of Haskell by outsiders a modest amount in a favourable
direction.

Thirdly, "effect systems" are used as a software engineering tool for
structuring programs in the large, but the name doesn't fit the use
case.  A talk by Tom Wells entitled "[Encoding Architecture Into Your
Code (So Agents Can't Break
It)](https://www.youtube.com/watch?v=JaLAvoyjwoQ)" describes how to
use an effect system to enforce layering in an application by defining
abstraction boundaries.  The executive summary is that *effect systems
can be used to constrain architecture*.  That's a concise way of
summarising a benefit of Haskell I've found when programming in the
large.  But the phrase sounds odd to me. What does architecture have
to do with effects?  "*Capability systems* can be used to constrain
architecture" sounds much more natural!  In fact it's so natural it's
almost obvious how that works: you give each component in your
architecture the capability to perform only what it needs to.  (A very
interesting open question remains: how do you use your capability
system to propagate capabilities to where they are needed?)

### Agent-generated code

Tom Wells's talk was specifically on the topic of maintaining good
architecture when code is generated by AI agents.  In a world where
the cost of generating technically correct code is declining rapidly
but good design is not becoming commensurately easier I believe that
effect systems have a critical role to play.  Their role is not
limited to architecture either: they can help enforce security
properties of code at scale.  I think the Haskell community will have
an easier time explaining how our favourite language helps in that
regard, and maybe win a few converts to our community, if instead of
calling them "effect systems" we call them "capability systems".

## References

* [Bluefin resources](../bluefin-all/)

* [Lambda
  Capabilities](https://roscidus.com/blog/blog/2023/04/26/lambda-capabilities/)
  by Thomas Leonard discusses capabilities in the context of OCaml.

[^1]: There are other effect systems that I call "synthetic" effect
    systems.  Examples of "synthetic" effect systems include
    `tranformers`, `extensible-effects` and `polysemy`, For more
    information on "`IO`-wrapper" or "analytic" effect systems, and
    "synthetic" effect systems, as well as the history of effect
    systems in Haskell in general, see my talk [A History of Effect
    Systems](https://www.youtube.com/watch?v=RsTuy1jXQ6Y)

[^3]: [Practical Principles for Computer
    Security](https://www.microsoft.com/en-us/research/publication/practical-principles-for-computer-security/)

[^4]: [Programming semantics for multiprogrammed
    computations](https://dl.acm.org/doi/10.1145/365230.365252)

[^5]: `extensible-effects` originates from [Extensible Effects: an alternative to Monad
    Transformers](https://okmij.org/ftp/Haskell/extensible/) by
    Kiselyov, Sabry and Swords

[^6]: The ealiest example of this idea of "exception scoped by a
    value" may have been Brachthäuser, Schuster, Ostermann in [Effects
    as Capabilities: Effect Handlers and Lightweight Effect
    Polymorphism](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/effekt-capabilitypassing-style-for-type-and-effectsafe-extensible-effect-handlers-in-scala/A19680B18FB74AD95F8D83BC4B097D4F).

[^7]: See also my article [Haskell's missing mutable reference
    type](https://h2.jaguarpaw.co.uk/posts/haskells-missing-mutable-ref/)

[^8]: Unless it's just "'pure' means 'referentially transparent'"

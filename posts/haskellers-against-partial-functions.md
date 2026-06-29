# Haskellers against partial functions

A collection of some of the objections to partial functions I have
come across.

* Article by Chris Done: [Deprecate Prelude.head and partial
  functions](https://chrisdone.com/posts/boycott-head/)

  > Please boycott Prelude.head and all its partial
  > friends. ... Please stop using partial
  > functions. Seriously. Remove them from your codebase.

* Article by Stephen Diehl: [Building a Better Custom Haskell
  Prelude](https://www.stephendiehl.com/posts/protolude/)

  > [Safe](https://hackage.haskell.org/package/safe) provides Maybe
  > versions of many of the various partial functions (`head`, `tail`)
  > that are shipped by default. Wrapping it up in a Maybe is widely
  > considered the right approach and if Haskell were designed today,
  > they would not be present.

  > ... partial functions which generally should be avoided

* Stackoverflow question: [Why 'head' (from the prelude package) has
  not been implemented the safe way with a
  Maybe?](https://stackoverflow.com/questions/53139761/why-head-from-the-prelude-package-has-not-been-implemented-the-safe-way-with)

  > I would love to see these unsafe functions tagged somehow with
  > some convention at least because I don't think any of us like when
  > an exception blows up in production :-)
  >
  > What prevents the community from fixing these issues in the prelude ?

* Discourse post: [Why are Partial Functions so prevalent in
  Prelude?](https://discourse.haskell.org/t/why-are-partial-functions-so-prevalent-in-prelude/8896)

  > why does the Prelude seem less safe than it should be?  ... It
  > confuses me why Prelude contains so many functions that may cause
  > an application to crash.

* Discourse post: [A modern take on the
  Prelude](https://discourse.haskell.org/t/a-modern-take-on-the-prelude/6619)

  > Some standard definitions are controversial: e.g. ... partial
  > functions such as head and tail

* [`relude` package](https://hackage.haskell.org/package/relude)

  > Usage of partial functions can lead to unexpected bugs and runtime
  > exceptions in pure code. The types of partial functions lie about
  > their behaviour. And even if it is not always possible to rely
  > only on total functions, relude strives to encourage
  > best-practices and reduce the chances of introducing a bug.

* [`rio` package](https://hackage.haskell.org/package/rio)

  > The RIO module works as a prelude replacement ...  removing common
  > "gotchas", like partial functions

* Reddit post: [Why are partial functions (as in `head`
  bad)?](https://old.reddit.com/r/haskell/comments/5n51u3/why_are_partial_functions_as_in_head_tail_bad/)

  > > The problem with partial functions is that they're liars. Consider
  > > head: its type is [a] -> a, which means "give me a list of as and
  > > I'll give you an a". So I give it [] - does it give me an a? No,
  > > it doesn't, it throws an exception instead.
  > >
  > > And when functions start lying about the things they return, you
  > can no longer reason about them.
  >
  > This is the best explanation, wow. I'm wondering why would haskell
  > then even allow it... it sounds very Haskell

  > In my opinion, one of the major selling points of functional
  > programming, and Haskell in particular, is that your functions are
  > side-effect free. Throwing an exception and exiting the program is
  > a pretty nasty side-effect. It effectively means every time you
  > call 'head' or 'tail', you need to analyze the whole program to
  > make sure the inputs are always valid

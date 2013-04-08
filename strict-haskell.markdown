# Strict Haskell

What would a strict version of Haskell look like?  That is, a pure language
with syntax as close to Haskell as possible but where function application
and data constructors are evaluated strictly.  Laziness should be accessible
through an explicit thunk datatype.

## Hughes

Would this language be worse than Haskell?  [Hughes] [hughes] talks about
the importance of laziness for modularity but I think that the issues he
raises only actually apply to lazy data structures.

## Augustsson

[Augustsson] [augustsson] raises some important points, though a lot of his
examples are based on the behaviour of "error" which in my opinion is
just wrong.  "error" is an effect and has no place in pure code.

### Lazy bindings

The "error" example is just wrong.  The "\a -> a + expensive" example can be
solved in a strict language through an explicit thunk data structures.

### Lazy functions

Augustsson's charge of lazy functions is a harder one to answer.  Again he
levels the false criticism of "error", but the point still stands.  I think
his idea of using {exp} for a thunk that when called evaluates exp is a good
one.

### Lazy constructors

I don't understand Augustsson's point about lazy constructors.  Does "lazy
constructor" mean "lazy datatype"?

### Cyclic data structures

I think these are solvable with explicit thunks.

### Reuse

I don't understand Augustsson's point here at all.  His "biggest gripe" is
about composability, but it seems trivially solvable with lazy data
structures.  He does seem to realise this: in the comments he says "It
doesn't matter if 'or' and 'map' are overloaded for different kinds of
collections, it's still wrong for strict collections".

Bob Harper makes an interesting point in the comments:

'In my mind however the conditional branch is not an example of [laziness].
The expression "if e then e1 else e2" is short-hand for a case analysis,
which binds a variable (of unit type) in each branch.  So the conditional is
not "lazy" at all; rather, it is an instance of the general rule that we do
not evaluate under binders (ie, evaluate only closed code).'

## Kmett

[Kmett] [kmett] raises some good points and some that I don't understand.

### Equational reasoning

I don't really understand how laziness (or probably more accurately,
non-strictness) is relevant to equational reasoning.  I would like to find
some explicity examples.  Perhaps "Theorems for Free"?

### Value restriction

I really don't understand the relevance of this at all.

### Code that can be understood in pieces

This is basically just lazy bindings.  I get why it's good, but would hope
to find a syntax which makes it less relevant.

### Elegant expression of certain algorithms

This is a lazy data structures only comment, as far as I can tell.

### Control structures

This is exactly the same as Augustsson's point above.

### Monads

Kmett claims monads only work with laziness.  I don't understand this.

## Being Lazy with Class

The "Being Lazy With Class" paper mentions two important uses of laziness:

* Recursive datastructures -- this is the same as Augustsson's "cyclic data
  structures argument" but better made.  Is this easily replaceable with a
  lightweight thunking syntax?
* Unusual control flow -- this is the same as Augusstson's point, and
  Kmett's "Control Structures" point

## Other

Don Syme [syme] notes in "Initializing Mutually Referential Abstract
Objects: The Value Recursion Challenge" that "Wadler et al.  describe
techniques to add on-demand computations to strict languages."

[cufp] notes that Mu has demonstrated strictness to be harmful to
modularity.

## What problems are we trying to solve?

* There are a lot of implicit thunks floating around in Haskell.  It is
  anathema to treat `A -> IO B` as if it were simply `A -> B` so it seems
  odd for `A` to mean a type of computations (returning values of type `A`)
  rather than itself a type of values.  Strict Haskell would be more honest
  since thunks would be represented by an explicit type.

* The need for `evaluate :: a -> IO a` is a wart which indicates that we do
  not understand how to mix `IO` and forcing thunks.

* When we have a function which releases some memory how can we ensure that
  the release happens before the rest of the program continues execution? 
  In the pure case it seems possible to use CPS and in IO it seems possible
  to use `evaluate` (and probably the CPS form too) but is there a unified,
  principled way?  It seems unlikely that we will find one whilst thunks are
  implicit.

* Lots of libraries (Lens, ST) have strict and lazy versions of functions. 
  Lots of monads have strict and lazy versions.  This is very confusing.  Is
  it really necessary?

## Drawbacks I anticipate with strictness

Note that monadic bind does not have an obvious drawback in a strict regime. 
With `m a -> (a -> m b) -> m b` the `m b` need not actually run anything if
the `m a` short circuits, because it's already hidden behind a function
(imagine `Maybe` for a prototypical case).

The `Applicative` instances will be potentially more problematic.  With
`(<*>) :: f (a -> b) -> f a -> f b` it will be hard (syntactically at the
very least) to stop the `f a` computation from running, even if the `f (a ->
b)` short circuits.  It will also only make sense to use lazy `Traversible`
instances.  Mapping an `Applicative` value creating function over an entire
`Traversible` is a waste when it is followed by a `sequence` which can short
circuit.  Perhaps the true nature of `(<*>)` is `LazyTuple (f (a -> b), f a)
-> f b`.

Is Haskell's `IO` itself lazy out of necessity?  Maybe.  Consider a pure
function containing an expression `let x = f y :: IO A`.  Then `x` is an
`IO` action ready to be run, but not run yet!  `IO` certainly contains some
sort of delaying and I don't understand the importance of this to the whole
strict Haskell issue.

[hughes] Why functional programming matters
[augustsson] http://augustss.blogspot.co.uk/2011/05/more-points-for-lazy-evaluation-in.html
[kmett] http://stackoverflow.com/questions/265392/why-is-lazy-evaluation-useful/265548#265548
[syme] http://research.microsoft.com/apps/pubs/default.aspx?id=79951
[cufp] http://anil.recoil.org/papers/2011-cufp-scribe-preprint.pdf

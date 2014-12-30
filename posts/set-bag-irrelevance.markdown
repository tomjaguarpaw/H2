# The Set/Bag Irrelevance

From time to time it is discussed whether SQL's support for duplicate
tuples in queries is useful and Codd et al's relational model is
lacking because it forbids them, or whether they make SQL a worse
language and the relation model is better off for eliminating them.
The former could be called "allowing bags".  Bags (or multisets) are
like sets except they permit repeated elements.

Date discusses the issue in the 22 pages of the "Date on Database"
chapter "Double Trouble, Double Trouble".  His objections to
duplicate tuples seem to be (paraphrasing)

1. "The existence of duplicate tuples implies the ability to
   distinguish identical things".  This objection I do not understand.
   He relates it vaguely to a philosophical concept of "The Principle
   of Indistinguishability of Indiscernables" though the reasoning is
   unclear.  Date claims (perhaps correctly) that there is no way to
   distinguish the "two" 6s that appear in the bag {3,6,6,8,8,8,11}
   but again it is unclear why any particular conclusion should follow
   from this.

2. "Relations are sufficient thus bags should be excluded for the sake
   of parsimony".  This argument I am susceptible to, but it crucially
   hinges on relations actually being sufficient.  More on this
   later; it's the crux of the issue!

## Why bags are nice

See [Multiset semantics](../multiset-semantics/).

## Sets and bags are equivalent

Despite all this handwringing it turns out that in a decent relational
query language (Date and Darwen might say "in a D") having set
semantics is exactly the same as having bag semantics.  That is, bags
can be implemented in terms of sets and vice versa.  This shouldn't be
surprising really but I don't think the consquences of this
observation have really been thought through.

Here's how you show the (trivial) correspondence.  Our decent
relational query language will have type abstraction so whereas Date
wants us to only use `Set (a, b, c, ...)` to represent a relation on
`a`, `b`, `c`, ... we are welcome to define

    type Bag t = Set (t, N+)

where `N+` is some previously defined domain type of strictly positive
integers.  With a bit of fiddling we can define conversions from `Set
t` to `Bag t` and vice versa (an adjunction or at least
adjunction-like) as well as project, extend, restrict, join and indeed
all our relational operators for the `Bag` type.  (Union is an
interesting case.  There are at least two sensible concepts of union
(sum and max) and it would probably be practical to provide both).

If on the other hand we were living in Date's nightmare bag-world we
would define

    type Set t = Bag t KEY t

That is, we define a `Set` to be be a `Bag` where the whole tuple is a
key.  This implies that there can exist at most one tuple of any given
value in the multiset, i.e. it is in fact a set.  As before we can
implement all the functionality for sets in terms of existing
functionality for bags.

## The argument is irrelevant

Imagine that the major proponent of a call by value lambda calculus
claims that the only tupling operation must be binary.  Detractors
claim "no we *must* have tuples of arbitrary size".  Once you have one
you have the other.  There's no point trying to forbid one option.

Thus we see that the argument about sets and bags is irrelevant.  If
we have a decent query language that allows some degree of type
abstraction we automatically have both if we have either.  It doesn't
matter whether Date thinks that Occam's razor implies we should work
with sets only.  We'll get bags too whether he likes it or not.

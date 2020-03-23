# Accidentally exponential in a compiler

## The introduction

I accidentally introduced exponential run time into a compiler I was
writing and did not realise for over three years.

## The language

Once upon a time I was writing an AST to represent expressions in a
programming language.  In reality the language was SQL, but for the
sake accessibility let's pretend it was a simple arithmetic language
that allows us to write zero, one, and (non-empty) sums and products.

```haskell
data Expr a = Zero a
            | One
            | Sum (NonEmpty (Expr a))
            | Prod (NonEmpty (Expr a))
```

The reason why `Zero` has an argument will be explained in due course.
First, let's implement wrapper functions to provide a nice API and
write an example expression.

```haskell
zero :: Expr ()
zero = Zero ()

(.+) :: Expr a -> Expr a -> Expr a
e1 .+ e2 = Sum (pure e1 <> pure e2)

(.*) :: Expr a -> Expr a -> Expr a
e1 .* e2 = Prod (pure e1 <> pure e2)

example :: Expr ()
example = (One .+ zero .+ zero .+ One) .* (zero .+ One .+ One)
```

We expect the example expression to evaluate to `2 * 2`, i.e. `4`.
Let's write an evaluator and check.  The evaluator will be recursive
on the structure of terms of type `Expr`.  We are good functional
programmers so we will write a data type and general fold function to
encapsulate the recursion pattern.  They basically write themselves,
given the definition of `Expr`.

```haskell
data ExprFold a r = ExprFold
  { zeroF :: a -> r
  , oneF  :: r
  , sumF  :: NonEmpty r -> r
  , prodF :: NonEmpty r -> r
  }

exprFold :: ExprFold a r -> Expr a -> r
exprFold f = \case
  Zero a  -> zeroF f a
  One     -> oneF f
  Prod es -> prodF f (fmap (exprFold f) es)
  Sum es  -> sumF f (fmap (exprFold f) es)
```

Now it's simple, almost tautological, to write an evaluator to
interpret terms in the integers.

```haskell
int :: Expr a -> Int
int = exprFold ExprFold
  { zeroF = const 0
  , oneF  = 1
  , sumF  = sum
  , prodF = product
  }
```

```
> int example
4
```

## The task

Now I'll explain the reason for the argument to the `Zero`
constructor.  The backend that we are targeting doesn't have a very
good concept of `Zero`!  We can happily send `One` to it but sending
`Zero` is quite tricky.  In reality the backend is SQL and it's not
terribly straightforward to generically represent an empty query in
SQL.

Instead of dealing with that problem directly, my approach was to give
`Expr` a type parameter.  Then expressions of type `Expr ()` can
contain `Zero` and expressions of type `Expr Void` cannot.

Expressions that might contain `Zero` need to have it removed before
the query is sent to the backend.  The following function removes all
occurrences of `Zero` from an expression.  If the expression itself
was actually zero it returns `Nothing`, otherwise it returns a `Just`.
Because the type parameter in the return type is polymorphic we have a
static guarantee that the result doesn't contain any `Zero`s!

```haskell
-- Just an expression, or Nothing.  Nothing means that the value
-- of the expression is zero.
removeZero :: Expr unit -> Maybe (Expr void)
removeZero = exprFold ExprFold
  { zeroF   = const Nothing
    -- ^ Zero is converted to Nothing
  , oneF    = pure One
    -- ^ One does not need to change
  , sumF    = \es ->
       Sum <$> nonEmpty (catMaybes (toList es))
  -- ^ We filter all zeros out of the Sum.  Terms that are zero
  -- (Nothing) are removed by catMaybes. If they were *all* zero
  -- then the argument to nonEmpty is [] so it returns Nothing.
  -- If some were not zero then we end up with a NonEmpty list of
  -- non-zeroes.
  , prodF   = \es ->
       Prod <$> (traverse removeZero =<< sequence es)
  -- ^ If any of the terms in a Prod is Nothing then the entire
  -- product is Nothing (because the product of a list
  -- containing zero is itself zero).
  }
```

```
>  fmap int (removeZero example)
Just 4
> fmap int (removeZero (zero .* One))
Nothing
```

We're only ever going to use `removeZero` at type

```haskell
removeZero :: Expr () -> Maybe (Expr Void)
```

but the more general type signature

```haskell
removeZero :: Expr unit -> Maybe (Expr void)
```

is valid, so let's use that instead.  After all, more polymorphism
gives fewer places for bugs to hide, right?


## The bug

`removeZero` calculates the correct result but regrettably it has
exponential run time!  How can this be?  The point of expressing
`removeZero` in terms of a fold function was to make it brainlessly
simple to write it efficiently and correctly.

The problem is that I called `removeZero` in the body of its own
definition.  I wasn't supposed to do that!  `exprFold` was supposed to
encapsulate all the recursion so that I need not use explicit
recursion myself.  For the `prodF` field I wrote

```haskell
prodF = \es -> Prod <$> (traverse removeZero =<< sequence es)
```

when I should have written

```haskell
prodF = \es -> Prod <$> sequence es
```

The argument to `prodF`, `es`, has *already* had the recursive call of
`removeZero` applied to each element by `exprFold`. I didn't need to
call `removeZero` again.  Calling it again doesn't change the result
(because there are no longer any zeros) but it doubles the amount of
work at each `Prod` node, making the run time exponential.

## The stars aligned

Ironically, if `removeZero` were less polymorphic, taking an argument
of type `Expr ()` instead of `Expr unit`, then the type checker would
have caught this bug.  The recursive call is at type `Expr void` which
Haskell's polymorphic recursion very happily allows to unify with
`Expr unit`.  Furthermore, if `removeZero` were not idempotent then
this bug would have been quickly discovered by the test suite.  The bug could only occur because the following three conditions coincided:

* `removeZero` is idempotent
* I chose to generalise the type signature
* Haskell supports polymorphic recursion

## The reality

This bug occurred in real life in
[Opaleye](https://github.com/tomjaguarpaw/haskell-opaleye), my
PostgreSQL
[EDSL](https://en.wikipedia.org/wiki/Domain-specific_language).  The
bug was [added in Feb
2016](https://github.com/tomjaguarpaw/haskell-opaleye/commit/784caea91f4715208a5e15e1528deb1f2a378191#diff-0da0d5fe745a7b37120073cdbb34a662R40-R41)
and only [removed nearly three and a half years
later](https://github.com/tomjaguarpaw/haskell-opaleye/commit/895a6781d8e95163ee6e82085eb56a12993012a0?diff=unified#diff-0da0d5fe745a7b37120073cdbb34a662L40-R40),
after being
[reported](https://github.com/tomjaguarpaw/haskell-opaleye/issues/434)
by [Christopher Sasarak](https://github.com/csasarak).  (Despite the
commit message the slowdown is exponential, not quadratic.  Suppose
that each level of the tree has only one branch.  We're running
`removeEmpty` twice for each branch, so `cost (Prod es) = 2 * sum
(cost es)`.)

## The conclusion

Opaleye makes good use of Haskell's type system to check correctness.
It has a comprehensive set of unit tests and property tests.  However,
because the buggy function returns the correct result there is no way
that Haskell's type system can spot the error.  Nor can unit tests,
property tests, Liquid Haskell, or any other system which only checks
the calculated value.  Some form of performance testing is required.
If you have a library without a performance testing suite perhaps you
will be lucky enough, like me, that your users act as performance
testers for you!

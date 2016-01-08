# Symbolic expressions can be Automatically differentiated too

Or, Automatic diffentiation demystified

All introductions to Automatic Differentiation that I have seen seem
to present the technique mysteriously.  It's actually very simple.
I'll describe how.

Since the very first time I learned calculus I have subconsciously
understood "differentiating" to mean symbolically manipulating a
expression to get another expression which represents its derivative.
I understood "calculating the derivative" to mean "differentiating"
followed by substuting in a value for a variable and reducing the
expression to get the value of the derivative.  "Differentiating" (2x
+ 1)^2 would give 4(2x + 1), and "calculating the derivative" of it at
8 involves substituting 8 for x to get 68.

Then I learned about [Automatic
Differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation)
(AD).  It claims to be able to calculate the derivative without
differentiating!  Given my subconscious understanding of what those
terms meant this seemed bizarre, impossible and magical.  It is
claimed it doesn't symbolically manipulate the expression at all.
Apparently it gets to 68 without going via 4(2x + 1).  How on earth
can this be possible?

It turns out that AD *is* extremely cool but it *is not* mysterious.
When I tried to understand it I was led astray by strange descriptions
like

> Forward-mode AD is implemented by a nonstandard interpretation of
  the program in which real numbers are replaced by dual numbers,
  constants are lifted to dual numbers with a zero epsilon
  coefficient, and the numeric primitives are lifted to operate on
  dual numbers. This nonstandard interpretation is generally
  implemented using one of two strategies: source code transformation
  or operator overloading.

If you try to read introductions to AD you'll probably come across a
lot of passages like this which describe the rigmarole of shoehorning
AD into a bog-standard proceduralish language, but they put an
additional veil on the *intrinsic* meaning of AD, rather than lifting
one.  You'll probably also get the impression that symbolic
expressions are bad.

AD is actually a very simple concept, and yes it can work on symbolic
expressions too.

If we have a type `E` of expressions of one variable `X` then we can
easily write an evaluator for it.

    {-# LANGUAGE LambdaCase #-}
    
    data E = X | One | Zero | Negate E
           | Sum E E | Product E E | Exp E
           deriving Show
    
    eval :: Double -> E -> Double
    eval x = ev where
      ev = \case
        X            -> x
        One          -> 1
        Zero         -> 0
        Negate e     -> -ev e
        Sum e e'     -> ev e + ev e'
        Product e e' -> ev e * ev e'
        Exp e        -> exp (ev e)
    
It's also easy to write a differentiator (implementing the usual rules
of calculus)

    diff :: E -> E
    diff = \case
      X            -> One
      One          -> Zero
      Zero         -> Zero
      Negate e     -> Negate (diff e)
      Sum e e'     -> Sum (diff e) (diff e')
      Product e e' -> Product e (diff e') `Sum` Product (diff e) e'
      Exp e        -> Exp e `Product` diff e
    
and we can write simple expressions.

    f :: E -> E
    f x = Exp (x `minus` One)
      where a `minus` b = a `Sum` Negate b
    
    smallExpression :: E
    smallExpression = iterate f X !! 3
    
    bigExpression :: E
    bigExpression = iterate f X !! 1000

Working with small expressions is easy

    > smallExpression
    Exp (Sum (Exp (Sum (Exp (Sum X (Negate One))) (Negate One))) (Negate One))
    
    > diff smallExpression
    Product (Exp (Sum (Exp (Sum (Exp (Sum X (Negate One))) (Negate
    One))) (Negate One))) (Sum (Product (Exp (Sum (Exp (Sum X (Negate
    One))) (Negate One))) (Sum (Product (Exp (Sum X (Negate One)))
    (Sum One (Negate Zero))) (Negate Zero))) (Negate Zero))
    
We can even differentiate them with `diff` and then evaluate their
derivatives with `eval`

    > mapM_ (print . flip eval (diff smallExpression))
            [0.0009, 1, 1.0001]
    0.12254834896191881
    1.0
    1.0003000600100016
    
but calculating the derivatives of big expressions this way is slow,
in fact quadratic in the size of the expression.

    > mapM_ (print . flip eval (diff bigExpression))
            [0.00009, 1, 1.00001]
    3.2478565715995278e-6
    1.0
    1.0100754777229357
    -- Trust me, that was slow

## The key idea of Automatic Differentiation

We can calculate derivatives efficiently (in time linear in the size
of the expression) by using the *key idea* of AD.  The *key idea* is
to evaluate both the expression *and* its derivative *at the same
time*.  I'm not going to go into why this is faster (it's to do with
avoiding redundant calculation) but suffice it to know that this is
indeed the unique, key idea of AD.

It's not even hard to write.  For each term, we evaluate the subterms
and the derivative of the subterms, and then combine them using the
usual rules of calculus.  They're exactly the same rules implemented
by `eval` and `diff` above, but encoded slightly differently.

    diffEval :: Double -> E -> (Double, Double)
    diffEval x = ev where
      ev = \case
        X            -> (x, 1)
        One          -> (1, 0)
        Zero         -> (0, 0)
        Negate e     -> let (ex, ed) = ev e
                        in (-ex, -ed)
        Sum e e'     -> let (ex, ed)   = ev e
                            (ex', ed') = ev e'
                        in (ex + ex', ed + ed')
        Product e e' -> let (ex, ed)   = ev e
                            (ex', ed') = ev e'
                        in (ex * ex', ex * ed' + ed * ex')
        Exp e        -> let (ex, ed) = ev e
                        in (exp ex, exp ex * ed)
    
Take, for example, the branch for `Exp`

        Exp e        -> let (ex, ed) = ev e
                        in (exp ex, exp ex * ed)

It says that the value of the exponential of `e` at `x` is `exp ex`
where `ex` is the value of `e` at `x` (this is utterly trivial) and to
calculate the derivative of the exponential of `e` at `x` we take `exp
ex * ed`, where `ed` is the value of the derivative of `e` at `x`
(this is just the definition of the derivative of the exponential
function).  That last sentence is a very long winded way of giving one
tautology, and one definition of the derivative of `exp`!  The former
is what is done in the `Exp` branch of `eval` and the latter is what
is done in the `Exp` branch of `diff`, only here the later is numeric
rather than symbolic.  Basically, there's nothing going on here.  Once
we have our key idea, everything else falls out for free; this whole
paragraph is a long way of saying nothing at all.

`diffEval` gives the same results as `diff` followed by `eval`

    > mapM_ (print . snd . flip diffEval smallExpression)
            [0.0009, 1, 1.0001]
    0.12254834896191881
    1.0
    1.0003000600100016

but is much quicker on large expressions

    > mapM_ (print . snd . flip diffEval bigExpression)
            [0.00009, 1, 1.00001]
    3.2478565715995278e-6
    1.0
    1.0100754777229357
    -- Trust me, it was fast

## The mystery of "dual numbers"

Even if, when you are reading an introduction to AD, you manage to
distinguish a nugget of theory amongst the grime of the implementation
details, you will probably still believe you need to write your
numerical calculations to work on "dual numbers", something like

    data D = Dual { value :: Double, derivative :: Double }

But as we've seen above, that too is an implementation detail.
Working with symbolic expressions is fine.  In fact what `diffEval`
does is give an interpretation of symbolic expressions `E` into dual
numbers `D`.

Mysterious comment: The distinction between `E` and `D` is exactly the
same as the distinction between a free monad and a hand-rolled monad
that contains the effects interpreted in a particular way.

## Conclusion

AD is a very cool idea and at its heart it's very simple.  There are
surely many important details that come later when you want to
optimize your AD implementation or extend it to higher dimensions, but
for the basics all you need is one key idea, and that is to calculate
the value derivative at the same time as the value of the expression.

## References

Jared Tobin wrote a [nice little
extension](http://jtobin.ca/ad-via-recursion-schemes/) using
catamorphism.  In fact if you use this technique then you can
implement `eval` and `diff` separately but still get good performance
when you compose them!  I may write about this later ...

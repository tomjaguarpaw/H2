# Why is naive symbolic differentiation slow?

In [another article about demystifying Automatic
Differentiation](../symbolic-expressions-can-be-automatically-differentiated)
(AD) I explained how to use the key idea of AD to calculate
derivatives in time linear in the size of a symbolic expression, after
mentioning that the naive approach is quadratic.  But why is the naive
approach quadratic?  In this article I'll explain.

Recall the `diffEval` function which implements the key idea of AD.

````haskell
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
````

If you look at how each expression node is handled you'll notice that
there is one recursive call per child node, leading to linear run
time.  An alternative implementation could have been this:

````haskell
diffEvalSlow :: Double -> E -> (Double, Double)
diffEvalSlow x = ev where
  ev = \case
    X            -> (x, 1)
    One          -> (1, 0)
    Zero         -> (0, 0)
    Negate e     -> let ex = fst (ev e)
                        ed = snd (ev e)
                    in (-ex, -ed)
    Sum e e'     -> let ex   = fst (ev e)
                        ed   = snd (ev e)
                        ex'  = fst (ev e')
                        ed'  = snd (ev e')
                    in (ex + ex', ed + ed')
    Product e e' -> let ex   = fst (ev e)
                        ed   = snd (ev e)
                        ex'  = fst (ev e')
                        ed'  = snd (ev e')
                    in (ex * ex', ex * ed' + ed * ex')
    Exp e        -> let ex = fst (ev e)
                        ed = snd (ev e)
                    in (exp ex, exp ex * ed)
````

`diffEvalSlow` has *two* recursive calls per child node, leading to
quadratic run time.

    > mapM_ (print . snd . flip diffEval bigExpression)
            [0.00009, 1, 1.00001]
    3.2478565715995278e-6
    1.0
    1.0100754777229357
    -- ^^ Trust me, it was fast

    > mapM_ (print . snd . flip diffEvalSlow bigExpression)
            [0.00009, 1, 1.00001]
    3.2478565715995278e-6
    1.0
    1.0100754777229357
    -- ^^ Trust me, it was slow
    
There is a similar reason for the slowness of the naive approach to
calculating the derivative of symbolic expressions.  The naive
approach is to first `diff` the expression, and then `eval` it.
`diff` and `eval` are as follows:

````haskell
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

diff :: E -> E
diff = \case
  X            -> One
  One          -> Zero
  Zero         -> Zero
  Negate e     -> Negate (diff e)
  Sum e e'     -> Sum (diff e) (diff e')
  Product e e' -> Product e (diff e') `Sum` Product (diff e) e'
  Exp e        -> Exp e `Product` diff e
````

You can see that, like `diffEval`, both of these functions have only
one recursive call per child node, leading to linear run time in the
size of their input.  So what's going on?  Why is composing them
quadratic?

Have a look at the `Product` case

````haskell
Product e e' -> Product e (diff e') `Sum` Product (diff e) e'
````

There are only two recursive calls to `diff`, but it *doubles* the
number of child nodes from two to four.  The key observation is

> The size of the output of `diff` can be quadratic in the size of the
  input!

`eval` then runs over this quadratic sized output, so the run time of
`eval` composed with `diff` can be quadratic in the size of the
original input.

The `Exp` case doubles the number of nodes too, as would potential
cases for `Sin`, `Cos` or any other function where the derivative is
given by the chain rule.

## Conclusion

The key idea of AD is to evaluate `e` and its derivative in *one*
recursive call, leading to linear run time, rather than *doubling* the
amount of work which needs to be done at each level, which leads to
quadratic run time.

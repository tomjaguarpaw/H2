# Reverse mode Automatic Differentiation

-- made simpler

## Warning: this article is old and rather idiosyncratic

I have a much better explanation of reverse mode AD at [Automatic
differentiation: source-to-source worked
examples](../automatic-differentiation-worked-examples.md).  I would
advise you not to read this article.  I'm only keeping it around for
historical interest.

In "[Symbolic expressions can be Automatically differentiated
too](../symbolic-expressions-can-be-automatically-differentiated)" I
demonstrated that forward mode Automatic Differentiation (AD) is not
as mysterious as it is often made out to be.  In fact it is quite
simple.

On the other hand, reverse mode AD seems unfortunately to be
unavoidably more complicated than forward mode, but in this article I
hope to show that it is still a bit simpler than it is often made out
to be.  If you read [introductions to reverse mode
AD](https://en.wikipedia.org/wiki/Automatic_differentiation#Reverse_accumulation)
you will find scary passages like

> reverse accumulation requires the storage of the intermediate
  variables wi as well as the instructions that produced them in a
  data structure known as a Wengert list (or "tape")

and

> The nodes in the adjoint graph represent multiplication by the
  derivatives of the functions calculated by the nodes in the
  primal. For instance, addition in the primal causes fanout in the
  adjoint; fanout in the primal causes addition in the adjoint; a
  unary function y = f(x) in the primal causes x̄ = ȳ f′(x) in the
  adjoint; etc.

This doesn't help anyone to learn reverse mode AD.  It *is* a bit
complicated but it *isn't* scary.

There is only any benefit in using reverse mode AD when
differentiating an expression with a large number of input variables.
If you only have a small number of input variables then forward mode
will probably be faster.  With that in mind let's set up the relevant
data structures and API.

## Expressions and vectors

````haskell
{-# LANGUAGE LambdaCase #-}

import qualified Data.Map.Strict as Map
import qualified Data.Maybe      as Maybe
import           Data.List       (foldl')

-- The type of coordinates.  This is for clarity of display
-- purposes only.
type Coord = Int

-- An expression type that has labels 'e' on subexpressions
-- and labels 'v' on variables.
--
-- The expression X1 * (X2 + (X1 * 2)) would be represented as
-- 
--     Var ((), 1)
--       `Product`
--     (Var ((), 2)
--        `Sum`
--      (Var ((), 1) `Product` (One `Sum` One)))
data E e v = Var     (v, Coord)
           | One
           | Zero
           | Negate  (e, E e v)
           | Sum     (e, E e v) (e, E e v)
           | Product (e, E e v) (e, E e v)
           | Exp     (e, E e v)
         deriving Show
````

Compare this to the forward mode expression datatype which had only
one variable ("`X`") and didn't allow labelling subexpressions or
variables.  We will use the labels in the reverse mode algorithm.

For the sake of simplicity we'll consider vectors of length 1000
represented by `Map`s.

````haskell
-- The vector type is implemented as a Map from coordinates
-- to Double.  In this example, for concreteness, we will consider
-- vectors of length 1000.
type V = Map.Map Coord Double

-- The zero vector
zero :: V
zero = Map.fromList (zip [1..1000] [0,0..])

-- Add two vectors
plus :: V -> V -> V
plus = Map.unionWith (+)

-- Multiply a vector by a scalar
times :: Double -> V -> V
times a = Map.map (a *)

-- Negate a vector
negateV :: V -> V
negateV = Map.map negate

-- The component of a vector in a given coordinate direction.
-- For example, the "component along" 2 of (3,4,5,6,...) is 4.
componentAlong :: Coord -> V -> Double
componentAlong i v = Maybe.fromMaybe 0 (Map.lookup i v)

-- A vector which has one non-zero entry, value x in the i
-- direction.  For example, "5 `inDirection` 3" is (0,0,5,0,...).
inDirection :: Double -> Coord -> V
inDirection x i = Map.fromList [(i, x)]

-- Add a quantity to the given component.  For example,
-- "plusComponent 2 10 (3,4,5,6,...)" is "(3,14,5,6,...)".
plusComponent :: Coord -> Double -> V -> V
plusComponent = Map.insertWith (+)
````

## Forward mode

We can use this API to implement forward mode AD for functions with
multiple inputs.

````haskell
forwardMode :: V -> E e v -> (Double, V)
forwardMode v = ev where
  ev = \case
    Var (_, i)             -> (componentAlong i v,
                               1 `inDirection` i)
    One                    -> (1, zero)
    Zero                   -> (0, zero)
    Negate (_, e)          -> let (ex, ed) = ev e
                              in  (-ex, negateV ed)
    Sum (_, e) (_, e')     -> let (ex, ed)   = ev e
                                  (ex', ed') = ev e'
                              in  (ex + ex', ed `plus` ed')
    Product (_, e) (_, e') -> let (ex, ed)   = ev e
                                  (ex', ed') = ev e'
                              in  (ex * ex', (ex `times` ed')
                                               `plus`
                                             (ex' `times` ed))
    Exp (_, e)             -> let (ex, ed) = ev e
                              in  (exp ex, exp ex `times` ed)
````

Compare this definition to the one of `diffEval` in the [the previous
article](../symbolic-expressions-can-be-automatically-differentiated).
You will see that it has exactly the same structure.  We can also
define a sample expression to test it on which corresponds precisely
to `bigExpression` from [the previous
article](../symbolic-expressions-can-be-automatically-differentiated).

````haskell
f :: E () () -> E () ()
f x = exp_ (x_ `minus` one)
  where a `minus` b = a `Sum` ((), Negate b)
        one         = ((), One)
        x_          = ((), x)
        exp_ a      = Exp ((), a)
        
bigExpression :: E () ()
bigExpression = iterate f x1 !! 1000
  where x1 = (Var ((), 1))
````

````haskell
exampleForward =
  mapM_ (print
         . componentAlong 1
         . snd
         . flip forwardMode bigExpression
         . (`inDirection` 1))
        [0.00009, 1, 1.00001]
````

    > exampleForward
    3.2478565715995278e-6
    1.0
    1.0100754777229357
    -- That was slow

Unfortunately, `forwardMode` is terribly slow.  In fact it takes time
proportional to *n*, where *n* is the number of inputs.  We're using
vectors of length 1,000 in this article and if we used vectors of
length 10,000 it would be ten times slower.  The reason for this
asymptotic complexity is that each time we combine the derivatives of
subexpressions, for example in

    ed `plus` ed'
    
we are doing *O(n)* work.

## Reverse mode

Reverse mode is faster when there are many inputs.  First I'll explain
what reverse mode is and then I'll explain why it's faster.
Unfortunately reverse mode is not as simple to explain as forward
mode.  It is simplest to understand if split into three parts.

Given a point in `V` at which to evaluate the derivative of our
expression first we decorate every subexpression with its *value* at
that point.

````haskell
evalDecorate :: V -> E e v -> (Double, E Double v)
evalDecorate v = ev where
  ev = \case
    Var (a, i)             -> (componentAlong i v, Var (a, i))
    One                    -> (1, One)
    Zero                   -> (0, Zero)
    Negate (_, e)          -> let (x, d1) = ev e
                              in  (-x,    Negate (x, d1))
    Sum (_, e) (_, e')     -> let (x, d1) = ev e
                                  (y, d2) = ev e'
                              in  (x + y, Sum (x, d1) (y, d2))
    Product (_, e) (_, e') -> let (x, d1) = ev e
                                  (y, d2) = ev e'
                              in  (x * y, Product (x, d1) (y, d2))
    Exp (_, e)             -> let (x, d1) = ev e
                              in  (exp x, Exp (x, d1))
````

The first component of the return value is the value of the whole
expression.  This evaluation is half of what forward mode AD does.  In
reverse mode we calculate the value but we do not combine it with the
derivative yet.  Instead we keep the whole decorated expression tree
around for a second pass.

By way of example, let us evaluate the partial derivatives of

    f = X1 * (X2 + (X1 * 2))

at `(X1, X2) = (3,4)`.  We expect to find that

* `f = 30`

* `f_X1 = X2 + 4 * X1 = 16`

* `f_X2 = X1 = 3`

After being decorated with values of subexpressions, `f` becomes

    [X1; 3] * [[X2; 4] + [[X1; 3] * [2; 2]; 6]; 10]

(From here we see indeed that `f = 3 * 10 = 30`.)

The second pass is the only conceptually hard part of reverse mode.
We propagate a "sensitivity" value from the root of the expression
tree to the variables.  It tells us how sensitive the value of our
expression is to changes of the variables.

````haskell
sensitivityDecorate :: Double -> E Double v -> E () Double
sensitivityDecorate = ev where
  ev s = \case
    Var (_, x)             -> Var     (s, x)
    One                    -> One
    Zero                   -> Zero
    Negate (_, e)          -> Negate  ((), ev (-s) e)
    Sum (_, e) (_, e')     -> Sum     ((), ev s e)
                                      ((), ev s e')
    Product (x, e) (y, e') -> Product ((), ev (s * y) e)
                                      ((), ev (s * x) e')
    Exp (x, e)             -> Exp     ((), ev (s * exp x) e)
````

The calculation simply follows the normal rules of calculus.  The
negation operation has sensitivity `-1` to its argument, the sum
operation has sensitivity `1` to both its arguments and the product
`x1 * x2` has sensitivity `b` to `x1` when the value of `x2` is `b`,
and sensitivity `a` to `x2` when the value of `x1` is `a`.

Continuing with our example, after sensitivity decoration we obtain

    [X1; 10] * ([X2; 3] + ([X1; 3 * 2] * 2))

which is

    [X1; 10] * ([X2; 3] + ([X1; 6] * 2))

The third step is trivial and is to walk the leaves and gather them
into a list.

````haskell
listOfVars :: [(v, Coord)] -> E e v -> [(v, Coord)]
listOfVars = ev where
  ev l = \case
    Var t                  -> t : l
    One                    -> l
    Zero                   -> l
    Negate (_, e)          -> l `ev` e
    Sum (_, e) (_, e')     -> l `ev` e `ev` e'
    Product (_, e) (_, e') -> l `ev` e `ev` e'
    Exp (_, e)             -> l `ev` e
````

Despite the functionality of `listOfVars` being trivial it is
*absolutely critical* that we gather the leaves by consing onto an
accumulating argument.  It is very expensive to use structurally
recursive list appends.

Our example gives us the following list of leaves

    [X1; 10], [X2; 3], [X1; 6]

Then we simply sum componentwise, giving

    [X1; 16], [X2; 3]

as anticipated.  Combining the passes gives the complete reverse mode
algorithm.

````haskell
reverseMode :: V -> E e v -> V
reverseMode v = foldl' (\d (s, x) -> plusComponent x s d) zero
                . listOfVars []
                . sensitivityDecorate 1
                . snd
                . evalDecorate v
````

On our previously defined example expression with vectors of size
1000, reverse mode is *much* faster than forward mode.

````haskell
exampleReverse =
  mapM_ (print
         . componentAlong 1
         . (\x -> reverseMode x bigExpression)
         . (`inDirection` 1))
        [0.00009, 1, 1.00001]
````

    > exampleReverse
    3.2478565715995362e-6
    1.0
    1.010075477722936
    -- Yes, it was fast

It is faintly reassuring that the answers we get are numerically
slightly different than for forward mode.  The algebraic derivative is
the same, of course, but the numerical operations are performed in a
different order which leads to tiny differences in the floating point
value.

Why is reverse mode faster than forward mode?  We noted previously
that forward mode must do *O(n)* work at each subexpression to combine
derivatives.  Reverse mode avoids this by walking the leaves.  In
reverse mode all the information you need about the derivatives is
contained in the leaves.

What are the downsides of reverse mode?  Firstly it's probably slower
than forward mode for small numbers of inputs, i.e. on small vectors.
Perhaps more importantly though the `evalDecorate` pass forces the
whole expression into memory which may be unacceptable for large
expressions.

Where is the Wengert list?  Presumably it's the expression tree
decorated with values of subexpressions which is then decorated with
sensitivities.  It seems nice to have this tree than a Wengert list
(or "tape")!  What's all this about adjoint graphs and duals and
primals?  I really have no clue.

## Conclusion

Reverse mode AD seems to be unavoidably more complicated than forward
mode but it is not actually particularly complicated and certainly not
as hard to understand as it seems to be from much of the literature
you will read about it.

Thanks to Edward Kmett who patiently answered my questions on reverse
mode so I was finally able to understand it.

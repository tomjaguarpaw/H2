# Haskell programs: how do they run?

If this article were entitled "Haskell programs: how do they look?" it
would be very short.  The answer would be "beautiful".  If it were
entitled "Haskell programs: what do they calculate?" I could write
about Haskell's lovely mathematical compositional behaviour.

But it's not; it's entitled "Haskell programs: how do they run?" and
explains how Haskell programs are executed step by step (specifically,
the programs produced by GHC).  Haskell is what is called a "lazy
language" and because lazy languages are rare the execution of Haskell
programs often remains rather mysterious even to its most frequent
users.  The consequence of this lack of understanding is programs that
have strange performance characteristics, often using vastly larger
amounts of memory than anticipated, and sometimes running more slowly
too.

This article will present a model that you can use to understand the
mechanism by which Haskell programs are executed.  It's not
complicated, but useful information on this topic is very hard to find
and it took me several years to put together all the key pieces of
information needed for a complete picture.  I'll reassemble the
complete picture for you much more quickly in the space of merely
several paragraphs below!

To fit in with the nomenclature in the Haskell literature I'm going to
use the word "evaluation" rather than "execution".  The latter seems
to better convey the notion of following a sequence of instructions
encoded by a program, but the former seems to be in wider use by those
who write compilers.

Firstly, let me mention some terms that are often used when talking
about how Haskell programs are evaluated: "redex", "weak head normal
form" (WHNF), "constant applicative form" (CAF), "call by need".  As
far as I can tell these are all terms from the mathematical theory of
lambda calculus and although they describe aspects of the process of
evaluation of Haskell programs they are not useful for understanding
the process of evaluation directly.  Two other phrases that are, in my
opinion, unhelpful and imprecise respectively, are "graph reduction"
and "delays the evaluation of an expression until its value is
needed".

## Normal form

By contrast I'm going to offer a very direct and concrete approach to
understanding how Haskell programs are evaluated.  The evaluation will
be done in terms of something I'm going to call the "normal form".
The normal form is a simplified version of Haskell with fewer language
constructs.  It has only

  * literals
  * variables
  * constructors
  * let
  * lambda
  * function application
  * case

Additionally, in a function call the function itself and arguments must
be variables or literals; and constructor applications must be
"saturated", i.e.\ there must be no missing arguments.

All Haskell programs can be translated into this normal form.  I'm not
going to expend space describing how because there's lots and lots of
Haskell syntax.  Suffice it to say that these can all be translated
straightforwardly to the normal form: where clauses, function
definition syntax, guards, nested patterns, nested expressions.

This normal form is a language with a direct operational reading.  You
could even call in an imperative language.  I'm not going to give a
special name to the normal form.  It's similar to "administrative
normal form" which is a concept from compiler development technology,
and it's also similar to STG, one of the GHC intermediate languages.

The evaluation of this normal form is sufficient for 99% of your
Haskell performance needs.  It will be enough to understand any
performance issues except the lowest level performance hackery.

Here's an example of a program in Haskell and its equivalent in the
normal form:

````haskell
-- Haskell
map f []     = []
map f (x:xs) = f x : map f xs

-- Normal form
map = \f xs -> case xs of
  []    -> []
  x:xs' -> let first = f x
               rest  = map f xs'
           in first : rest
````

Notice that we can't use Haskell's multi-clause function definition
syntax, the arguments must specifically be in lambdas and we can't
have nested subexpressions; instead we have to bind subexpressions to
variables.

## What do we evaluate to?

Since we are considering a process for evaluating Haskell programs our
first thought ought to be about what we expect from the process: what
do Haskell programs evaluate *to*?

The result of an evaluation is called a "value", and a value is either

 * a (fully saturated) constructor (including primitive types), or
 * a lambda

(This is the same concept that is captured by the notion of WHNF.)

## How do we evaluate

How exactly does evaluation proceed?  It's actually rather imperative.

* literals, lambdas, constructors -- these are already values and
  therefore evaluation terminates immediately

* `let x = e in body`: create a closure for `e` on the heap and let
    `x` be a pointer to this closure, i.e.\ all mentions of `x` scoped
    by this binding point to that closure.  A closure is basically a
    block of code, along with some variables, that can be evaluated
    later.

* variables `x`: `x` is a pointer to a closure, as described above.
    Evaluate that closure to a value and overwrite its memory location
    with the value (the overwriting stage is sometimes called
    "memoization").

* function application `f a`: first evaluate `f`.  This results in
    something of the form `\x -> body`, evaluate `body` after
    substituting `a` for `x` within it.

* `case e of alts`: evaluate `e`, then check which alternative matches
    and evaluate it.

Those are the complete rules for evaluating Haskell programs.  Some
examples will follow shortly.

## Resource usage

There are just two simple rules which fully explain how these
evaluation rules cause usage of memory resources.

Firstly, the only thing that allocates on the heap is `let`.

Secondly, the only thing that consumes stack (in a way that we care
about) is `case`, whilst it is evaluating its scrutinee.  (The
"scrutinee" is `s` in `case s of ... -> ...`.) Haskell consumes stack
whilst evaluating `s` so it can know where it should continue from once
evaluation of `s` has finished.

Nothing else consumes any memory resources.

## Evaluation example

We'll trace an example implemented in terms of `map` above plus the
following two functions

````haskell
-- Haskell
repeat x = xs where xs = x : xs

-- Normal form
repeat = \x -> let xs = x : xs
               in xs

-- Haskell
head (x:xs) = x

-- Normal form
head = \xs -> case xs of x:xs' -> x
````

The expression to evaluate will be

````haskell
-- Haskell
head (map (\x -> x + x) (repeat (10 + 1)))

-- Normal form
let f = \x -> x + x
    t = 10 + 1
    r = repeat t
    m = map f r
in head m
````

To save space in this article you can watch [my talk at Haskell
eXchange
2016](https://skillsmatter.com/skillscasts/8726-haskell-programs-how-do-they-run)
where I trace the evaluation of this program.

### Primitive operations

Something to bear in mind is that primitive operations must run on
values, i.e. fully evaluated things.  This means that an operation
such as `(+)` must be implemented as something like

````haskell
(+) = \x y -> case x of
     x' -> case y of
         y' -> primitive_plus x' y'
````

where `primitive_plus` is perhaps implemented by some underlying C
library.  The consequence is that `(+)`, or any other primitive
operation evaluates its arguments.

## Sharing

The rules for evaluation tell us exactly what we need to know about
how sharing happens.  Consider these two versions of an enumeration
function:

````haskell
enum1 = zip ns
    where ns = [1..]

enum2 xs = zip ns xs
    where ns = [1..]
````

If we're familiar with the concept of "eta equivalence" we might think
that these two definitions will be evaluated in the same way.  We
might think of an analogous definition where two definitions *are*
equivalent and evaluated in the same way:

````haskell
inc1 x = (+) 1 x

inc2   = (+) 1
````

But the enumeration examples do not evaluate the same way.  Why not?
Let's look at what happens when we convert them into the normal form

````haskell
let enum1 = let ns = [1..]
            in zip ns

let enum2 = \xs -> let ns = [1..]
                   in zip ns xs
````

Considering how these evaluate we see that in `enum1`, `ns` is shared by *all*
invocations of the function, whereas in `enum2`, `ns` is created afresh
by each invocation of the function.  Sharing `ns` can lead to large
and surprising space leaks.

So we see that careful consideration of how Haskell programs evaluate
can shed light on unexpected or surprising behaviour.

## `foldl`

`foldl` is expressed in the normal form as

````haskell
foldl = \f z xs -> case xs of
  []    -> z
  x:xs' -> let z' = f z x
           in foldl f z' xs'
````

See [my talk at Haskell eXchange
2016](https://skillsmatter.com/skillscasts/8726-haskell-programs-how-do-they-run)
where I trace the evaluation of `foldl (+) 0 [1..100]`.  In short, it
builds up the proverbial "long chain of thunks" on the heap before
consuming O(n) stack space to evaluate them.

## `foldl'`

`foldl'` is the "strict version of `foldl`" and is expressed in the
normal form as

````haskell
foldl' = \f z xs -> case xs of
  []    -> z
  x:xs' -> case f z x of
      z' -> foldl' f z' xs'
````

(As a short aside, note that in Haskell we would use `seq` instead of
`case` because a Haskell `case` with no patterns doesn't evaluate its
scrutinee.)

See [my talk at Haskell eXchange
2016](https://skillsmatter.com/skillscasts/8726-haskell-programs-how-do-they-run)
where I trace the evaluation of `foldl' (+) 0 [1..100]`.  Unlike `foldl`, it
consumes neither (much) heap nor stack.

## `foldr`

`foldr` is expressed in the normal form as

````haskell
foldr = \f z xs -> case xs of
  []    -> z
  x:xs' -> let rest = foldr (+) 0 xs'
           in f x rest
````

See [my talk at Haskell eXchange
2016](https://skillsmatter.com/skillscasts/8726-haskell-programs-how-do-they-run)
where I trace the evaluation of `foldr (+) 0 [1..100]`.  It consumes
O(n) heap and stack.

## Conclusion

All Haskell programs can be translated straightforwardly to a simple
normal form which has a simple imperative-style interpretation.

By following through the execution of the program we can understand
how it uses memory resources.

## Addendum

After writing this article I was pointed to the following references,
which perhaps make better reading:

* [A transformation-based optimiser for
   Haskell](https://www.microsoft.com/en-us/research/wp-content/uploads/1998/09/comp-by-trans-scp.pdf),
   section 3.1

* [Let-floating: moving bindings to give faster
  programs](https://www.microsoft.com/en-us/research/wp-content/uploads/1996/05/float.pdf),
  section 2.1

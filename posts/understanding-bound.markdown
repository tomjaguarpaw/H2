# Understanding Bound

WARNING: INCOMPLETE!

## Introduction

[`bound`](http://hackage.haskell.org/package/bound-1.0.6) is a Haskell
library written by Edward Kmett that allows you to encode and
manipulate languages that involve binding, such as lambda calculus.
Edward has published [an introduction to `bound` at the FP Complete
School of Haskell](https://www.fpcomplete.com/user/edwardk/bound).
His introduction starts by summarizing other approaches to dealing
with binding before explaining in detail how `bound` was derived from
an earlier approach.  I'm not sure who came up with the idea of bound
but Edward is listed as the sole author on [the Hackage
page](http://hackage.haskell.org/package/bound-1.0.6).

I'll give a very brief refresher on untyped lambda calculus.  It's not
supposed to be an introduction.

Lambda calculus lets us build "terms" by following three rules.  We
can

  * write variables, such as `x`, `y`, `z`
  * apply one term to another, so from `x` and `y` we get `x y`
  * abstract a term with a lambda, so from `x y` we can get
    `\x (x y)` and `\y (x y)`

For notational convenience, instead of writing `(x y) z` we will often
drop the parentheses and write simply `x y z` instead.

Besides these rules for forming terms there are important operations
which manipulate terms.

### Substitution

Given terms `t1` and `t2` we can "substitute `t1` for all free
occurrences of `x` in `t2`".  For example

  * If `t1 = z z` and `t2 = y x` then the result is `y (z z)`.  One
    occurence of `x` has received the substitute.
  * If `t1 = \y y` and `t2 = z x x` then the result is `z (\y y) (\y
    y)`.  Two occurrences of `x` have received the substitute.
  * If `t1 = z` and `t2 = \y (y z)` then the result is `t2 = \y (y
    z)`.  There were no occurrences of `x` so the substitution left
    the term with the same form.

  * If `t1 = y z` and `t2 = (x y) (\x z x)` then the result is `t2 =
    (y z y) (\x z x)`.  This is important!  The first occurrence of
    `x` was not bound by any lambda so it is called "free" and
    receives the substitutute.  The second occurrence *was not* free
    so it *does not* receive the substitute.

### Instantiation

A term of the form `\x ...` can have its binding instantiated to a
variable.  For example, in `\x (x y x)` the lambda abstraction can be
instantiated with `x`, giving `x y x`.  But the `x` in the lambda was
arbitrary, so we can also instantiate it with `z`, giving `z y z`.


When we embed something like the lambda calculus in Haskell we want a
type that represents terms of this form








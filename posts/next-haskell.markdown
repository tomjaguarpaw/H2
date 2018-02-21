# Next Haskell

What changes would we want to make to Haskell, but we can't for fear
of breaking backwards compatibility?  They should instead be made to
the Next Haskell.

* Better names for `data`, `type` and `newtype`: [https://mail.haskell.org/pipermail/haskell-cafe/2015-August/120732.html](https://mail.haskell.org/pipermail/haskell-cafe/2015-August/120732.html)

* `main :: IO (); main = ...` involves duplication.  This needs to be
  got rid of, but I can't think of a good way right now.

* Reduce special syntax: no special syntax for list types or list
  literals, maybe not even for tuple types or tuple literals!  `(->)`
  should not be a reserved identifier.

* Either do notation or comprehensions, not both.

* Remove `if`.  The only version of `case` should be a "lambda case".
  An arguments can be passed to it using the normal means of function
  application.

* Either arguments should be taken on the left hand side of a
  function, or the function type constructor should be reversed.

* No unary function `-` for negation.

* `:` and `::` should be swapped

* No partial record fields

* x `f` y should desugar to f y x

* Warnings that should be errors

    * Missing patterns

    * Missing record fields

* Get rid of `otherwise`.  It's far too cute.

## Sources of ideas

* https://www.reddit.com/r/haskell/comments/4f47ou/why_does_haskell_in_your_opinion_suck/

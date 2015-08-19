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

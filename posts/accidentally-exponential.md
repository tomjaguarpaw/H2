# Accidentally exponential in a compiler

WIP

See

* https://github.com/tomjaguarpaw/haskell-opaleye/issues/434

* https://github.com/tomjaguarpaw/haskell-opaleye/commit/895a6781d8e95163ee6e82085eb56a12993012a0?diff=unified

Despite the commit message the slowdown is actually exponential.
Suppose that each level of the tree has only one branch.  We're
running removeEmpty twice at each level, so cost(n) = 2 * cost(n-1).

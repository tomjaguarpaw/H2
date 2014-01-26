# `seq`

In Section 10.3 of ["A History of Haskell: Being Lazy with
Class"](http://research.microsoft.com/en-us/um/people/simonpj/Papers/history-of-haskell/history.pdf)
it is explained that `seq` originally required a typeclass.  This
requirement was removed when it required too many changes to type
signatures when speculative `seq`s were introduced when debugging.

* An [interesting discussion of `seq` and eta
  equivalence](http://cstheory.stackexchange.com/questions/19165/is-eta-equivalence-for-functions-compatiable-with-haskells-seq-operation/19175#19175)

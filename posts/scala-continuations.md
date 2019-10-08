# Scala continuations

WIP

https://www.scala-lang.org/files/archive/api/2.11.12/scala-continuations-library/#scala.util.continuations.package

* `A @cpsParam[-B, +C]` corresponds to `(a -> m b) -> m c` in
  Haskell.  The shift and reset operations in the Scala docs
  correspond to the ones in the [Haskell
  docs](https://www.stackage.org/haddock/lts-12.1/transformers-0.5.5.0/Control-Monad-Trans-Cont.html)
  modulo the Haksell ones not being indexed, and some extra `lift`s or
  `return`s being inserted or eliminated.

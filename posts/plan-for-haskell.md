# A plan for Haskell

* Improve performance of GHC
    * In time and memory usage

* Make cabal’s api usable

* Stop cabal rebuilding pandoc and aeson

* Improve important libraries:
    * Improve their API
    * Document their API
    * Write tutorials

* Banish lists

* Banish space leaks
    * Strict data
    * Force on the way out not the way in
    * Strict containers
    * Nested strict data
    * Get people to take seriously that programs must perform will in
      space and time without optimisations

# A plan for Haskell

* Improve performance of GHC
    * In time and memory usage

* Cabal
  * Make cabal’s command line API usable
  * Stop cabal rebuilding pandoc and aeson so frequently

* Improve important libraries:
    * Improve their API
    * Document their API
    * Write tutorials

* Banish space leaks
    * Strict data
    * Force on the way out not the way in
    * Strict containers
    * Nested strict data
    * Get people to take seriously that programs must perform well in
      space and time without optimisations

* Banish lists

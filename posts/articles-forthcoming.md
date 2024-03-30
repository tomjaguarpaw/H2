# Article forthcoming

* `HasCallStack` considered harmful

  Using `HasCallStack` it is possible to write

  ```
  myInt :: HasCallStack => Int
  ```

  that has a "different value" depending on where in your program it
  is called.  This seems undesirable!

  <https://github.com/NorfairKing/haskell-dangerous-functions/issues/17>

* Bluefin streams finalize promptly, more complex example

  Prompted by elaforge, an example of mixing streams with prompt
  finalization

  <https://discourse.haskell.org/t/solving-a-resourcet-related-space-leak-in-production/11007/11?u=tomjaguarpaw>

* Bluefin streams don't leak space

  Unlike lazy lists or the streaming library, Bluefin streams don't
  leak space

  <https://github.com/haskell-streaming/streaming/issues/110#issuecomment-1518988421>

* Internal versus external streams in Bluefin

  Looking at the iterator dichotomy of "push/internal" vs
  "pull/external" through a Bluefin lens.

  <https://medium.com/@pateldhruv020/pull-vs-push-system-ab95a65b0938>

  <https://lobste.rs/s/isx2ju/go_s_weird_little_iterators>

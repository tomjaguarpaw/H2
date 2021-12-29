# How I use HLS and Emacs LSP mode

* Seems faster than dante on small codebases, though slower on big
  ones.

## How to use

* Turn on HLS/LSP in a buffer: `M-x lsp`

* Turn off HLS/LSP in a buffer (and in all buffers using the same
  server): `s-l w q`

* Restart HLS/LSP in a buffer: `s-l w r` (although I'm not sure this
  really works.  Turn off and then turn on seems to be better.)

(`s-` is the "super" key and can be simulated with `C-x @ s`)

## What's great

Use an identifier that's not imported.  HLS offers to import it for
you.  I don't think it can offer to import things that are not in any
visible package, which has pros and cons.

## Papercuts

* I don't know how to find all buffers in which LSP is running (in
  order to turn them off).  The buffer called `*lsp-log*` gives some
  clues).

* Have to write out a `hie.yaml` cradle all the time with `cradle:\n
  cabal:`, because cradle autodetection in broken.

* Wingman doesn't give choices: `foo n1 n2 = _` doesn't give choice of
  `n1` or `n2`. Just fills with `n1`.

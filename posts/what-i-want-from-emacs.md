# What I want from emacs

* I want `C-x b` to *always* go back to the last buffer I was in.  By
  default it does not if another window is already visiting that
  buffer.

* Helm

    * Tab completion: it should complete up to the longest common
      prefix, not choose the currently selected

    * Helm: tying `filename` matches `FileName`.  Typing
      `dir/filename` should match `Dir/filename`.

* When I'm visiting a buffer whose file has been `git mv`'d, offer to
  move the buffer to it.

* Select containing expression

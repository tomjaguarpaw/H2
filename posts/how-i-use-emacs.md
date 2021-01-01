# How I use emacs

* Launch the emacs server.

  ```
  PATH=~/.ghcup/bin:$PATH emacs --fg-daemon
  ```

  I think this may require version at least 26.  If there are things
  that ought to be in your `PATH` (for me `cabal` installed by ghcup)
  this is a convenient way to ensure that they are there.

* Launch the emacs client as a long-running editor

  ```
  emacsclient --create-frame
  ```

  To close this frame you will have to do `C-x 5 0`
  (`save-buffers-kill-terminal` will also work).

* Launch the emacs client here in this terminal (for use as a Git
  commit message editor, etc.)

  ```
  emacsclient --create-frame --alternate-editor=jmacs [filenames]
  ```

  You can put this in the `EDITOR` environment variable. It will fall
  back to `jmacs` as an alternative editor if the emacs server is not
  running.

  If no filenames were provided then you can use `C-x 5 0` to close
  the frame.  If filenames were provided then you also have the option
  to close the frame by killing all their buffers.

* Git blame

  <https://stackoverflow.com/questions/15460550/git-blame-with-commit-details-in-emacs>
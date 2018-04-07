# Dante for dummies

I wanted to get started with [Dante: Emacs mode for Interactive
Haskell](https://github.com/jyp/dante) but found it difficult for
various reasons.  Here's how I managed it.

1. Get Dante

    I just downloaded `dante.el` so I can run it with `M-x
    eval-buffer`.

1. Use Emacs 25

    It doesn't seem to work with Emacs 24

1. Get dependent packages

    * It requires `lcr`. It also requires `attrap` if you want autofix.

          I don't know how to use Emacs properly so I just went to
          [MELPA](https://melpa.org/) and downloaded the files.  You
          can load them with `M-x eval-buffer`.

    * It also required some other stuff like `dash` which I managed to
      install with `apt`.

        I don't want to use MELPA if I don't have to.

1. Turn on Dante and flycheck minor modes

    You should probably use mode hooks to turn these on but I just used

    * `M-x dante-mode`

    * `M-x flycheck-mode`

1. If it complains about missing `stack`

    Dante tries to work out what sort of project you are working in so
    it know how to launch GHCi.  The way it works this out is through
    `dante-repl-command-line-methods-alist`.  It checks for
    `stack.yaml` before `.cabal`.  I have both but I want to use cabal
    so I had to delete the stack entry.  I used `M-x customize-group`
    to do this, in the "Dante Repl Command Line Methods Alist"
    setting.

Then it seems that Dante just works.

* 'M-g n' and 'M-g p' go to the next and previous errors respectively.
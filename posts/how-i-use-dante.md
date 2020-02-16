# How I use dante

## Introduction

[Dante](https://github.com/jyp/dante) is an "Emacs mode for
Interactive Haskell" (that is, an IDE) with a high power-to-weight
ratio, being easy to configure, reliable and featureful.  I will take
you on a brief tour of how I use Dante, starting with installation and
configuration, demonstrating the features, and explaining how to
resolve some typical problems.

Dante works by spawning an interactive ghc process (ghci, cabal repl,
stack repl), loading your code in it, and talking to it to find out
all sorts of information regarding types, errors, warnings, suggested
fixes, and more.

## Installation

The most difficult part of setting up dante was learning how to
effectively manage Emacs packages.  The only sane way that I have
found is to use
[`straight.el`](https://github.com/raxod502/straight.el/).  It
provides some hope that one can define a reproducible package
environment.  Thus to install dante I do the following.

* Install `straight.el`

  I use the snippet in "[Bootstrapping
  `straight.el`](https://github.com/raxod502/straight.el/blob/develop/README.md#bootstrapping-straightel)".

* Install `use-package`

  I use the following line, from "[Integration with
  `use-package`](https://github.com/raxod502/straight.el#integration-with-use-package)"
  
```lisp
(straight-use-package 'use-package)
```


* Install `dante` and `attrap` (with `straight.el`-using `use-package` stanzas)

```lisp
(use-package attrap
  :straight t
  )

(use-package dante
  :straight t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode)
  )
```

  (These are the default install commands from the
  [dante](https://github.com/jyp/dante#installation) and
  [attrap](https://github.com/jyp/attrap#attrap-emacs-mode-to-fix-the-flycheck-error-at-point)
  installation guides, adapted to use `straight.el` and slightly
  simplified.)

## Dante's features

* Red-squiggly type checking

  Dante provides underlines for errors and warnings (via
  [flycheck](https://www.flycheck.org/en/latest/)) .  You can navigate
  them with
  
  * `C-c ! n`: next error/warning
  * `C-c ! p`: previous error/warning
  * `C-c ! l`: list errors/warnings

* Automatic error fixing

  Dante can automatically repair a large number of errors and
  warnings.  For example, if I have omitted the type signature of a
  top-level binding then I place the point over it, type `C-c /`, and
  the type signature appears!
  
  * `C-c /`: apply ghc suggestion at point
  
  Dante has the ability to
  
  * automatically fill value and type holes
  * add missing type signatures and add missing constraints to type signatures
  * add missing imports and extensions
  * rename mistyped, and remove unused, variables, types, imports and constraints
  
  The full list of repairable conditions can be found under `def
  attrap-ghc-fixer` in
  [`attrap.el`](https://github.com/jyp/attrap/blob/master/attrap.el).
  
* Info at point

  Dante can tell you some basic info about the code under the point,
  or within the region.

  * `C-c .`: type of selection
  * `C-c ,`: info at point (including site of definition)

* Definition and use sites

  Dante can jump to the definition site of an identifier and find all the
  uses of a definition.
  
  * `M-.`: go to definition
  * `M-?`: find uses

* Completion

  Dante interfaces with [company
  mode](https://company-mode.github.io/) to provide completion
  suggestions.
  
  Personally I'm not familiar with company mode as I tend to use the
  extremely low-tech but reasonably useful `M-/`
  ([dabbrev-expand](https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html)).
  Unfortunately, Company mode doesn't seem to be able to complete
  identifiers defined in the current module which makes it rather less
  useful than it might otherwise be.


## Problems

* dante can get stuck.

  It forgets what it's doing and gives either no errors or meaningless
  errors.  Sometimes dante thinks its temporary file (with a name like
  `/tmp/dante9eOz40.hs`) is your real source file.  This seems to
  happen to me if I change a source file underneath dante, for example
  with `git reset --hard`.

  To fix this problem I do `M-x dante-restart` and re-save the file.
  Then it becomes unstuck.

* Dante cannot add missing type signatures with constrains

  Unfortunately dante doesn't seem able to add a missing type
  signature if the type signature requires constraints (for example,
  `Monad m => m Int`).  In that case you have to manually add a
  partial type signature `foo :: _` and then fill in the hole.
  
  This is probably a ghc issue rather than a dante issue, per se.

* I wish it were easier to understand the diagnostics from
  `dante-diagnose`.

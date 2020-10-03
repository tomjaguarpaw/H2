# What I don't like about Emacs

I wanted to add to a hook so that `whitespace-mode` is enabled
whenever I edit makefiles.  `M-x describe-mode` says that

> In addition to any hooks its parent mode might have run, this mode
runs the hook ‘makefile-gmake-mode-hook’, as the final or penultimate
step during initialization.

I added the following line to `.emacs`

```
(add-hook 'makefile-gmake-mode 'whitespace-mode)
```

and ran `M-x eval-region` on the new line.  But `whitespace-mode` did
not become enabled when editing makefiles!  This puzzled me for
several frustrating minutes before I realised that I had used the
wrong hook name.  I should have written

```
(add-hook 'makefile-gmake-mode-hook 'whitespace-mode)
```

What I really want in this situation is a *type* error.  Of course,
I'm never going to get one when writing Emacs lisp.  I would settle
for a run time error, but neither was one of those forthcoming.  Why
on earth not?!

A [StackOverflow question and
answer](https://stackoverflow.com/questions/21104788/why-does-add-hook-allow-hook-to-be-void)
hint at the underlying rationale. Emacs supports setting hook
variables for packages that have not been loaded yet.  It must be
possible to add hooks to arbitrary symbols, even incorrectly spelled
ones!  To me this seems to be a consequence of Emacs's 1970s roots; we
hadn't yet learned that programming via big blobs of global mutable
state should be avoided.

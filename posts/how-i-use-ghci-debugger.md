# How I (don't) use the GHCi debugger

[GHCi has a
debugger](https://downloads.haskell.org/ghc/8.10.1/docs/html/users_guide/ghci.html#the-ghci-debugger)
which I would use if it supported the things I wanted to do, which is
to analyse space leaks.  Unfortunately it seems to have two
limitations which make it essentially unusable for this purpose.

1. The only bindings it shows are those of the free variables of the
   expression being evaluated.  This behaviour is so restricted that I
   find it hard to understand how anyone can use the GHCi debugger for
   *any* non-trivial purpose.

   > We originally provided bindings for all variables in scope, rather
   > than just the free variables of the expression, but found that
   > this affected performance considerably, hence the current
   > restriction to just the free variables.

   [GHCi didn't have a debugger in version
   6.6.1](https://downloads.haskell.org/ghc/6.6.1/docs/html/users_guide/ghci.html)
   and by [version the next version (6.8.1) this restriction was
   already in
   force](https://downloads.haskell.org/ghc/6.8.1/docs/html/users_guide/ghci-debugger.html#ftn.id3136849).
   It's not completely clear how hard they've tried to remove this
   restriction.

2. It doesn't show (as far as I can tell) any information about the
   structure of an unevaluated closure.  In fact, this seems to be not
   a weakness of the debugger specifically but GHCi generally.  The
   `:print` command doesn't show the structure of closures.

   ```
   > let foo = [1,2 :: Int]
   > let bar = foo ++ foo
   > :print bar
   bar = (_t20::[Int])
   ```

   I really want to see something like the following, otherwise the
   debugger is completely useless for diagnosing space leaks.  We need
   to see when we're building up large unevaluated thunks!

   ```
   > let foo = [1,2 :: Int]
   > let bar = foo ++ foo
   > :print bar
   bar = (foo ++ foo::[Int])
   ```

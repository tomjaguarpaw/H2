# How I use cabal

-- version 3.2.0.0.

## I want to download cabal

I use [ghcup](https://www.haskell.org/ghcup/).  Personally I prefer
the [manual
install](https://gitlab.haskell.org/haskell/ghcup-hs#manual-install).
I am on a 32-bit Linux system so I do

```
curl -f https://downloads.haskell.org/~ghcup/0.1.9/i386-linux-ghcup-0.1.9 > ~/ghcup
chmod +x ~/ghcup
```

Then to install cabal it is a simple matter of

```
~/ghcup install cabal 3.2.0.0
```

The binary that it installs will end up at
`~/.ghcup/bin/cabal-3.2.0.0` and a symlink to it will probably be
created at `~/.ghcup/bin/cabal`.

## I want to play around with a package in the REPL

For very quick exploration you can specify a dependency on the
command line. If you want something that is a little more persistent
you can create a temporary project and add it as a dependency.

### Specify a dependency on the command line

In a directory that doesn't contain a Haskell package's `.cabal` file
(perhaps your home directory) run

```
cabal v2-repl --build-depends QuickCheck
<it downloads and builds QuickCheck if necessary...>
*Main> import Test.QuickCheck
*Main Test.QuickCheck> let prop h t = (length t + 1) == (length $ h:t)
*Main Test.QuickCheck> quickCheck prop
+++ OK, passed 100 tests.
```

### Create a temporary project and add it as a dependency

    $ mkdir my-temporary-package
    $ cd my-temporary-package
    $ cabal init

Edit `my-temporary-package.cabal` to change

    build-depends:       base >=... && <...

to

    build-depends:       base, QuickCheck

(The constraints on base are going to cause you more harm than good
for a quick exploratory project. You can add them back later if it
becomes necessary.)

    $ cabal v2-repl

### What about `cabal install --lib`?

I would avoid it.  To me it seems like a fools errand to try to load
non-system packages with anything but `cabal`.

## I have a package in local source directory. I want to use it in another local package.

Suppose I have a package called `package1` in a local source directory
and whose cabal file is at `/home/me/package1/package1.cabal`; I want
to use it in another local package I'm developing whose cabal file is
`/home/me/package2/package2.cabal`.

I achieve this by adding an `optional-packages` stanza to `package2`'s
`cabal.project`, as follows.

* If `/home/me/package2/cabal.project` already exists

  just add the following line

  ```
  optional-packages: /home/me/package1/package1.cabal
  ```

* If `/home/me/package2/cabal.project` does not yet exist

  create `/home/me/package2/cabal.project` containing the following

  ```
  packages: *.cabal
  optional-packages: /home/me/package1/package1.cabal
  ```

  It is vital that the "`packages: *.cabal`" line be included
  otherwise your project defaults to containing no packages at all.
  This default doesn't seem to make any sense, but there it is.

### `cabal install --lib` doesn't seem to work

Although `cabal install --lib` seems like it ought to support use
cases like this, I've never been able to get it to work.

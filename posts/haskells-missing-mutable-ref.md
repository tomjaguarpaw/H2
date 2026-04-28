# Haskell's missing mutable reference type

Haskell is missing a mutable reference type.  It is a reference type
similar to what Java calls a *scoped value*, Python a *context
variable* and Common Lisp a *"special" variable*.

The idea is that you would have a type (call it `IOScopedRef a`) which
is a reference to an `a` than you can mutate in `IO`.  It would differ
from
[`IORef`](https://www.stackage.org/haddock/lts-24.38/base-4.20.2.0/Data-IORef.html#t:IORef)
in that any mutations to it are only visible within a particular
scope. That is, to modify an `IORef` you have

```.hs
modifyIORef :: IORef a -> (a -> a) -> IO ()
```

whereas with `IOScopedRef` you would have

```.hs
modifyIOScopedRef :: IOScopedRef a -> (a -> a) -> (IO r -> IO r)
```

In practice you get different sort of behaviour, for example with
`IORef`

```.hs
modifyIORef ref (const "hello")
i1 <- readIORef
-- i1 == "hello"
do
   i2 <- modifyIORef ref (const "world")
   -- i2 == "world"
   ... no more modifies ...

i3 <- readIORef ref
-- i3 == "world"
```

versus with `IOScopedRef`

```.hs
modifyIOScopedRef ref (const "hello") $ do
  i1 <- readIOScopedRef ref
  -- i1 == "hello"
  modifyIOScopedRef ref (const "world") $ do
    i2 <- readIOScopedRef ref
    -- i2 == "world"
    ... regardless of whether more modfies occur here ...

  i3 <- readIOScopedRef ref
  -- i3 == "hello"
  ...
```

That is to say, when exiting a `modifyIOScopedRef` block, the value of
`ref` would be reset to whatever it was when entering it. This would
happen regardless of whether the block was left by normal termination
or by abnormal termination (exception).





* *Haskell*

  * **type**:  `IOScopedRef`
  * **create**: `withIOScopedRef init (\ref -> ...)`
  * **modify**: `modifyIOScopedRef ref f action`
  * **read**: `readIOScopedRef ref`
  * **details**: <https://github.com/ghc-proposals/ghc-proposals/pull/751#issuecomment-4295909948>

* *Java*

  * **type**:  `ScopedValue`
  * **create**: `ScopedValue.where(KEY, init)`
  * **modify**: `ScopedValue.where(KEY, x).run(() -> ...)`
  * **read**:  `KEY.get()`
  * **details**:
    <https://docs.oracle.com/en/java/javase/25/core/scoped-values.html>

* *Python*
  * **type**:  `ContextVar`
  * **create**:  `var = ContextVar("v")`
  * **modify**:  `token = var.set(x)` + `try: ... finally: var.reset(token)`
  * **read**: `var.get()`
  * **details**: <https://docs.python.org/3/library/contextvars.html>

* *Common Lisp*
  * **type**: special (dynamic) variable defined via `defvar` (by
    convention the variable name starts and ends with `*`)
  * **create**: `defvar *v* init`
  * **modify**: `(let ((*v* x)) ...)`
  * **read**: `*v*`
  * **details**: <https://www.lispworks.com/documentation/lcl50/aug/aug-109.html>

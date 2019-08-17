# Functional looping

Explicit looping is commonplace in imperative programming languages.
For example, this is how I might write a loop in Python.

```python
wrongAttempts = 0

while True:
    print("Enter password")
    s = input()
    if s != "letmein":
        wrongAttempts += 1
        print("You have been incorrect", wrongAttempts, "times")
    else:
        break

print("You are into the system!")
```

We can always replace imperative looping with recursion.

```haskell

main = do
  let myLoop wrongAttempts = do
        putStrLn "Enter password"
        s <- getLine
        when (s /= "letmein") $ do
          let wrongAttempts' = wrongAttempts + 1
          putStrLn
            (  "You have been incorrect "
            ++ show wrongAttempts'
            ++ " times"
            )
          myLoop wrongAttempts'

  myLoop 0
  putStrLn "You are into the system!"
```

There are a couple of small differences between the Python version and
the Haskell version.  In the Haskell version we explicitly continue by
naming `myLoop` whereas the Python version explicitly `break`s
instead, and naturally the Haskell version explicitly passes the state
for the next iteration.

There are also two bigger differences that make the Haskell version
clumsier.  Firstly we have to name `myLoop` and call it explicitly.
Secondly the initial state (`0`) is passed in far from the definition
of `myLoop`.

How can we fix this?  Let's define a genereric looping combinator,
`loop`.  To make my loop less clumsy I want to avoid naming `myLoop`
and I want to pass the initial state in to `loop` directly.  This
suggests I want to write something like

```haskell
loop 0 (\continue wrongAttempts -> do
   ...
   continue wrongAttempts')
```

where the loop name `continue` is now bound in a lambda.

Can we implement this?  Well, `loop` will have to look like

```haskell
loop state body = body ... state
```

but what can I pass as the second argument to `body`?  It needs to be
something that takes the updated state and continues with the loop.
But that's just `flip loop body`!  So I can define

```haskell
loop state body = body (flip loop body) state
```

and then I can write `main2` which doesn't have the original
clumsinesses.

```haskell
main2 = do
  loop
    0
    (\continue wrongAttempts -> do
      putStrLn "Enter password"
      s <- getLine
      when (s /= "letmein") $ do
        let wrongAttempts' = wrongAttempts + 1

        putStrLn
          (  "You have been incorrect "
          ++ show wrongAttempts'
          ++ " times"
          )
        continue wrongAttempts'
    )
  putStrLn "You are into the system!"
```

## Cryptic remarks

Note that `flip loop body state = body (flip loop body) state` so
`flip loop body = body (flip loop body)`.  If we write `fix f = f (fix
f)` then `loop = flip fix`.  N.B. [`fix` is normally
written](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Function.html#v:fix)
as `fix f = let x = f x in x` for efficiency.

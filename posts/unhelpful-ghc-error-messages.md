# Unhelpful GHC error messages

## "Or perhaps you meant to enable BlockArguments?"

Here's a simple Haskell program with a silly error.  The `bar = 3`
line is indented one space too few.

```haskell
main = do
  let foo = 1
     bar = 3

  print (foo + bar)
```

Under GHC 8.4 I get the helpful error message

```
bad-block-args.hs:3:6: error: parse error on input ‘bar’
  |
3 |      bar = 3
  |      ^^^
```

but under GHC 8.6 I get the completely baffling and useless error
message

```
bad-block-args.hs:1:8: error:
    Unexpected do block in function application:
        do let foo = 1
    You could write it with parentheses
    Or perhaps you meant to enable BlockArguments?
  |
1 | main = do
  |        ^^...
```

No, I didn't mean to enable `BlockArguments`.  I was not a supporter
of the `BlockArguments` extension in the first place, and now it is
providing me with *negative* value when I'm not even trying to use it.
I have had
this occur to me a few times in the wild, always in an unwieldy `do`
block that is difficult to understand even when GHC isn't complaining
about a syntax error.  I would like GHC's *help* here not a reference
to an unrelated [small syntax
extension](https://osa1.net/posts/2020-01-22-no-small-syntax-extensions.html).

Under GHC 8.10 I get an error message in the correct place, but *also*
the unhelpful and misleading error about enabling `BlockArguments`.
Ah well, that's better than nothing.

```
badd.hs:1:8: error:
    Unexpected do block in function application:
        do let foo = 1
    You could write it with parentheses
    Or perhaps you meant to enable BlockArguments?
  |
1 | main = do
  |        ^^...

badd.hs:3:10: error:
    parse error on input ‘=’
    Perhaps you need a 'let' in a 'do' block?
    e.g. 'let x = 5' instead of 'x = 5'
  |
3 |      bar = 3
  |          ^
```

# Resolving git rebase conflicts

How to resolve git rebase conflicts

You will be presented with two different hunks.  Your task is to
combine the logical changes of the two options.

It is almost always the wrong thing to choose exactly one of them.

Suppose we start with this Python file

```python
def foo1():
    print("foo1")

def foo2():
    print("foo2")

def foo3():
    print("foo3")

def foo4():
    print("foo4")

def foo5():
    print("foo5")

def main():
    foo1()
    foo2()
    foo3()
```

## Adding two different things

Suppose I have a patch to add a call of `foo4` and another to
add a call of `foo5`, that is

```diff
 def main():
     foo1()
     foo2()
     foo3()
+    foo4()
```
and

```diff
 def main():
     foo1()
     foo2()
     foo3()
+    foo5()
```

and I try to rebase the latter on the former the result is a conflict.
It's important to use `git -c merge.conflictStyle=diff3 rebase` (or
`merge`)

```diff
++<<<<<<< HEAD
 +    foo4()
 ++||||||| merged common ancestors
 ++=======
 +     foo5()
 ++>>>>>>> Add foo 5
```

The commit you are transplanting expected to see the state of the file
in the middle and to change it to the state of the file in the bottom
hunk.  Instead what it saw was the state at the top.

The correct way to resolve this conflict is to combine the logical
change of the bottom hunk relative to the middle (adding `foo5`) and
the logical change of the top hunk relative to the middle (adding
`foo4`) (in your editor this will typically be easier to do by making
that change to the top hunk), that is, we want to end up with the file
we save in our editor as

```python
def main():
    foo1()
    foo2()
    foo3()
    foo4()
    foo5()
```
which corresponds to a diff of
```diff
 def main():
      foo1()
      foo2()
      foo3()
      foo4()
+     foo5()
```
(different from the earlier addition of `foo5` diff because it
contains `foo4` in its context.

## Removing two different things

If I have one branch which removes `foo2` and another which removes `foo3`, that is

```diff
def main():
     foo1()
-    foo2()
     foo3()
```
and
```diff
 def main():
     foo1()
     foo2()
-    foo3()
```

and I try to rebase the latter onto the former the conflict is


```diff
      foo1()
++<<<<<<< HEAD
 +    foo3()
++||||||| merged common ancestors
+     foo2()
++    foo3()
++=======
++    foo2()
++>>>>>>> Remove foo3
```

This means that the rebased commit "Remove foo3" (top) expected to be
applied to a context where `foo2` was still present (middle), but in
the base branch (top) `foo2` had been removed.  To resolve we should
apply the logical intent of the rebased commit (bottom) to the base
branch (top).

```diff
      foo1()
```


## Thanks

https://codeinthehole.com/guides/resolving-conflicts-during-a-git-rebase/

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

The correct way to resolve this conflict is to apply the logical
change of the bottom hunk (adding `foo5`) to the state of the top hunk
beyond what the bottom patch expected (the addition of `foo4`) (in
your editor this will typically be easier to do by making that change
to the top hunk), that is, we want to end up with the file we save in
our editor as

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


## Thanks

https://codeinthehole.com/guides/resolving-conflicts-during-a-git-rebase/

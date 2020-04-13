# Resolving git rebase conflicts

How to resolve git rebase conflicts

You will be presented with two different hunks.  Your task is to
combine the semantic content of the two patches.

It is almost always the wrong thing to choose exactly one of them.
The whole point of merging is to *combine* two different changes, not
to choose one or the other.

The existence of a merge conflict means that the patches could not be
merged textually.  However, the aim is actually to merge them
*semantically*.  Merge tools are rarely (perhaps never) clever enough
to be able to perform semantic merges so they settle for textual
merges.  In the cases where textual merges are not possible the task
is left to human ingenuity.

Key takeaway: The aim is to merge the *semantic* content of the two
patches.

## General summary

1. `git -c merge.conflictStyle=diff3 rebase ...`

   (the `diff3` conflict style option shows important information in
   the conflict markers that would otherwise be absent)

2. For each conflict, observe the logical change that the rebased
   commit was trying to make.

   * Either look at the difference between the middle hunk and the bottom
     hunk of the marked conflict, or

   * look at `git show REBASE_HEAD`

3. Observe the state of the base branch.

   * Either look at the top hunk of the marked conflict, or

   * look at `git show HEAD:<filename>`

4. Apply the logical change that the rebased commit was trying to make
   to the state of the base branch.

   This will probably be easiest to do by editing the top hunk of the
   conflict.

5. `git rebase --continue`


## Example file

Let's work with this example Python file

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

## Worked example: Adding two different things

Suppose I have a patch that adds a call of `foo4` and another patch
that adds a call of `foo5`, that is

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

If I try to rebase the latter on the former then the result is a
conflict.

```diff
      foo1()
      foo2()
      foo3()
++<<<<<<< HEAD
 +    foo4()
++||||||| merged common ancestors
++=======
+    foo5()
++>>>>>>> Add foo 5
```

### The intent of the rebased commit

There are two equivalent ways to see the intent of the rebased commit.

* The difference between the middle hunk (below "merged common
  ancestors") and the bottom hunk (above the commit description, "Add
  foo 5")

* The output of `git show REBASE_HEAD` (this is generally easier to
  read, but more verbose as it also contains diffs relating to
  non-conflicting parts of the patch).  In this case it shows

```
     foo1()
     foo2()
     foo3()
+    foo5()
```

Using either method you can see that the logical change of the commit
you are rebasing is to add `foo5()` after `foo3()`.

### Resolving the conflict

There are two equivalent ways to see the state of the branch that you
are rebasing onto (the "base branch").

* The top hunk (below `HEAD`)

* The output of `git show HEAD:<filename>` (this is probably less
  useful because it shows the entire state of `<filename>` without
  drawing your attention to the conflicting section).  In this case

```
...
def main():
    foo1()
    foo2()
    foo3()
    foo4()
```

Either way, you can see that the base branch has `foo4` after `foo3`.

The correct way to resolve this conflict is to apply the logical
change of the rebased commit (adding `foo5` after `foo3`) to state of
the base branch (which has `foo4` after `foo3`).  In your editor this
will typically be easiest to do by making the necessary change to the
top hunk and then deleting the other hunks.  It requires semantic
understanding to know exactly which way of resolving the resolution is
satisfactory, if any.  For example, we could put `foo5()` before or
after `foo4()`.  In this case a natural resolution might be

```python
def main():
    foo1()
    foo2()
    foo3()
    foo4()
    foo5()
```

Once this change has been made the file can be `git add`ed and the
rebase can continue via `git rebase --continue`.

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
branch (top).  That is we should end up with

```diff
      foo1()
```


This is the commit you are trying to port

```
git show REBASE_HEAD
```

and this is how your port is currently looking

```
git diff HEAD
```




## Thanks

https://codeinthehole.com/guides/resolving-conflicts-during-a-git-rebase/

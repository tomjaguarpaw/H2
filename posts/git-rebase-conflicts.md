# Resolving git rebase conflicts

## A note about terminology

This article is about `git rebase` conflicts.  A similar article with
slightly different technical details could be written for `git merge`
conflicts.  I use "merge" as a non-technical term for combining the
content of two different repository states, not to refer to `git
merge`ing specifically.

## Introduction

When a git rebase conflict occurs you will be presented with a
conflict region (or "hunk") that shows why the rebased commit couldn't
be applied to the base branch.  To resolve a rebase conflict, your
task is to apply the logically-intended (i.e. semantic) change of the
rebased commit to the base branch.

Some merge tools (for example [Emacs
SMerge](https://emacs.stackexchange.com/questions/16469/how-to-merge-git-conflicts-in-emacs/16470#16470))
offer you the option to "keep their changes" or "keep our changes".
This is almost always wrong.  The whole point of merging (remember,
I'm using that word in the non-technical sense, not to refer to `git
merge`ing specifically) is to *combine* two conflicting things, not to
choose one over the other.  By choosing one you ignore a change in the
other that may be required by other parts of the same commit (parts
that may not see because they merged without conflict!).

The existence of a conflict means that the patches could not be merged
textually.  However, the aim is actually to merge them *semantically*.
Merge tools are rarely (perhaps never) clever enough to be able to
perform semantic merges so they settle for textual merges.  In the
cases where textual merges are not possible the task is left to human
ingenuity.

Key takeaway: the aim is to apply the logical (i.e. semantic) content
of the rebased commit to the base branch.

## Summary of the conflict resolution procedure

Resolving a rebase conflict requires understanding the logically
intended (semantic) change of the rebased commit and applying it
manually to the base branch.  Here is the procedure to follow.  The
details will be explained by way of example later.


1. Issue the `git rebase` command.

   Use the `diff3` conflict style option because it shows important
   information in the conflict markers that would otherwise be absent.

   ```
   git -c merge.conflictStyle=diff3 rebase ...
   ```

2. For each conflict, observe the logical change that the rebased
   commit was intended to make.

   * Either look at the difference between the middle hunk section and
     the bottom hunk section of the marked conflict, or

   * look at `git show REBASE_HEAD`

3. Observe the state of the base branch at each conflict region.

   * Either look at the top hunk section of the marked conflict, or

   * look at `git show HEAD:<filename>`

4. Apply, to the base branch, the logical change that the rebased
   commit was intended to make.

   This will probably be easiest to do by editing the top hunk section
   of the conflict and then deleting the others.

5. Add the file and continue the rebase, with `git add` and

   ```
   git -c merge.conflictStyle=diff3 rebase --continue
   ```

## Why use `diff3`?

Resolving conflicts is literally impossible using the default merge
conflict style. I suggest setting `diff3` in your global options.

```
git config --global merge.conflictStyle diff3
```

Why is it literally impossible?  Consider a branch which adds a
function `foo1` at a particular source file location

```python
def foo1():
    print("foo1")
```

and another branch which adds a function `foo2` at the same location

```python
def foo2():
    print("foo2")
```

If I rebase one on the other I get a conflict.  The default merge
conflict style will show

```diff
++<<<<<<< HEAD
 +def foo1():
 +    print("foo1")
++=======
+ def foo2():
+     print("foo2")
++>>>>>>> Add foo2
```

What are the conflict markers telling me?  They're telling me that I
need to *add* both `foo1` and `foo2` to the file, right?
Unfortunately not!  Consider a file in which `foo1` and `foo2` already
exist, and two branches, one of which removes `foo1` and one of which
removes `foo2`.  If I rebase one on the other what is the result?  The
default merge conflict style will show

```diff
++<<<<<<< HEAD
 +def foo1():
 +    print("foo1")
++=======
+ def foo2():
+     print("foo2")
++>>>>>>> Remove foo1
```

Under the default conflict style the case of removing two functions is
indistinguishable from the case of adding two functions
(besides the text of the commit message which can only ever be a
hint)!  Therefore it is insufficient for purpose of
resolving conflicts.  This probably explains why resolving conflicts
is seen as a dark art.  `diff3` not only makes it possible, below we
will see that it is often easy.

## Example file

The examples will based on changes to this example Python file.

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

## Worked example: adding two different things

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
conflict.  The conflict is marked in the file by conflict markers, as
below.  This is sometimes referred to as a "hunk".  There are three
sections in the hunk:

1. Between `<<<` and `|||`: the state of the base branch, i.e. what
   the rebase commit actually saw when it tried to apply its change

2. Between `|||` and `===`: the state that the rebased commit
   expected to see

3. Between `===` and `>>>`: what the result of applying the rebased
   commit would have been, if it had seen what it expected

These three sections are always different from each other.  If any
pair were the same then there would be no conflict!


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

There are two equivalent ways to see the logical intent of the rebased
commit.

* It is the difference between the middle hunk section (below "merged common
  ancestors") and the bottom hunk section (above the commit description, "Add
  foo 5").

* It is shown in the output of `git show REBASE_HEAD`.  This is
  generally easier to interpret than the above, but more verbose as it
  also contains diffs relating to non-conflicting parts of the commit.
  In this case it shows

```diff
     foo1()
     foo2()
     foo3()
+    foo5()
```

Using either method you can see that the logical change of the rebased
commit is to add `foo5()` after `foo3()`.

### The state of the base branch

There are two equivalent ways to see the state of the base branch.

* The top hunk section (below `HEAD`)

* The output of `git show HEAD:<filename>`.  This latter form is
  probably less useful than the former because it shows the entire
  state of `<filename>` without drawing your attention to the
  conflicting section.  In this case it shows

```
...
def main():
    foo1()
    foo2()
    foo3()
    foo4()
```

Either way, you can see that the base branch has `foo4()` after
`foo3()`.

### Resolving the conflict

The correct way to resolve this conflict is to apply the logical
change of the rebased commit---adding `foo5()` after `foo3()`---to
state of the base branch---which has `foo4()` after `foo3()`.  In your
editor this will typically be easiest to do by making the change to
the top hunk section and then deleting the other hunk sections.  It
requires semantic understanding to know exactly which way of resolving
the conflict is satisfactory, if any.  For example, we could put
`foo5()` either before or after the appearance of `foo4()`.  Let's
choose the latter and add `foo5()` below `foo4()` in the top hunk
section (i.e. below `HEAD`).

```diff
      foo1()
      foo2()
      foo3()
++<<<<<<< HEAD
 +    foo4()
 +    foo5()
++||||||| merged common ancestors
++=======
+    foo5()
++>>>>>>> Add foo 5
```

and then delete the middle and bottom hunk sections, and the conflict
markers, to get

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

## Worked example: removing two different things

Suppose I have a patch that removes the call of `foo2` and another patch
that removes the call of `foo3`, that is

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

If I try to rebase the latter onto the former the conflict is


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

### The intent of the rebased commit

By looking at the difference between the middle hunk section and the
bottom hunk section, or by looking at `git show REBASE_HEAD`---which
shows

```diff
 def main():
     foo1()
     foo2()
-    foo3()
```

---we can see that the intent of the rebased patch was to remove
`foo3()`.

### The state of the base branch

On the other hand, the hunk section under `HEAD`, and `git show
HEAD:<filename>`---which is

```
def main():
    foo1()
    foo3()
```

---show that the base branch does not contain `foo2()`, but `foo3()`
is still there, ripe for removal.  We need to apply the logical intent
of the rebased patch to this context, which is done by removing the
appearance of `foo3()` in the hunk section below `HEAD`

```diff
      foo1()
++<<<<<<< HEAD
++||||||| merged common ancestors
+     foo2()
++    foo3()
++=======
++    foo2()
++>>>>>>> Remove foo3
```

and then we delete the middle and bottom hunk sections and the
conflict markers to get

```
def main():
    foo1()
```

## Worked example: one addition, one removal

If I rebase the addition of `foo4()` on the removal of `foo3()` the
conflict is

```
  def main():
      foo1()
      foo2()
++<<<<<<< HEAD
++||||||| merged common ancestors
++    foo3()
++=======
+     foo3()
+     foo4()
++>>>>>>> Add foo4
```

`git show REBASE_HEAD` shows

```
     foo1()
     foo2()
     foo3()
+    foo4()
```

so the intention of the rebased commit is to add `foo4()`.  Doing this
in the top hunk section (i.e. below `HEAD`) leads to

```
  def main():
      foo1()
      foo2()
++<<<<<<< HEAD
 +    foo4()
++||||||| merged common ancestors
++    foo3()
++=======
+     foo3()
+     foo4()
++>>>>>>> Add foo4
```

and after deleting the deleting the other hunk sections we are left with

```
def main():
    foo1()
    foo2()
    foo4()
```

## Worked example: a renaming and a removal

Suppose I rename `foo1` to `foo1_new_name` like so

```diff
-def foo1():
+def foo1_new_name():
     print("foo1")

...

 def main():
-    foo1()
+    foo1_new_name()
     foo2()
     foo3()
```

and rebase the removal of `foo2()` (as above) on top.  Then the
conflict markers show

```
  def main():
++<<<<<<< HEAD
 +    foo1_new_name()
 +    foo2()
++||||||| merged common ancestors
+     foo1()
++    foo2()
++=======
++    foo1()
++>>>>>>> Remove foo2
      foo3()
```

and `git show REBASE_HEAD` shows

```
def main():
     foo1()
-    foo2()
     foo3()
```

Therefore to apply the logical intent of the rebased commit I need to
remove `foo2()`.  Doing so in the top hunk section gives

```
  def main():
++<<<<<<< HEAD
 +    foo1_new_name()
++||||||| merged common ancestors
+     foo1()
++    foo2()
++=======
++    foo1()
++>>>>>>> Remove foo2
      foo3()
```

and after deleting the other hunk sections and conflict markers we are
left with

```
def main():
    foo1_new_name()
    foo3()
```

## Worked example: a renaming and an extraction

Suppose I rebase the patch changing `foo1` to `foo1_new_name` as above
onto a commit which combines `foo1` and `foo2` into a new function
called `foo1and2` like so

```
 def foo2():
     print("foo2")

+def foo1and2():
+    foo1()
+    foo2()
+
 def foo3():
     print("foo3")

...

 def main():
-    foo1()
-    foo2()
+    foo1and2()
     foo3()
```

The conflict is

```
  def main():
++<<<<<<< HEAD
 +    foo1and2()
++||||||| merged common ancestors
++    foo1()
++    foo2()
++=======
+     foo1_new_name()
+     foo2()
++>>>>>>> Rename foo1 to foo1_new_name
      foo3()
```

What was the logical intent of the rebased commit?  `git show
REBASE_HEAD` shows

```
-def foo1():
+def foo1_new_name():
     print("foo1")

...

 def main():
-    foo1()
+    foo1_new_name()
     foo2()
     foo3()
```

so the logical intent is to rename `foo1` to `foo1_new_name`.  This
poses a conundrum.  There is no way to make this change within the
conflict region itself!  The call of `foo1` has been moved into a
different function.  We must make the change there instead, leading to
a final result of

```
def foo1and2():
    foo1_new_name()
    foo2()

...

def main():
    foo1and2()
    foo3()
```

## Worked example: a semantic conflict

Suppose we renamed `foo1` as above and then tried to rebase upon it a
commit that renames `foo1` to something else.  The conflict would look
something like

```
++<<<<<<< HEAD
 +def foo1_new_name():
++||||||| merged common ancestors
++def foo1():
++=======
+ def foo1_another_new_name():
++>>>>>>> Rename foo1 again
      print("foo1")

...

  def main():
++<<<<<<< HEAD
 +    foo1_new_name()
++||||||| merged common ancestors
++    foo1()
++=======
+     foo1_another_new_name()
++>>>>>>> Rename foo1 again
      foo2()
      foo3()
```

`git show REBASE_HEAD` shows

```
-def foo1():
+def foo1_another_new_name():
     print("foo1")

...

 def main():
-    foo1()
+    foo1_another_new_name()
     foo2()
     foo3()
```

that is, the logical change is that `foo1` is renamed
`foo1_another_new_name`.  On the other hand, `git show
HEAD:<filename>` shows

```
def foo1_new_name():
    print("foo1")

...

def main():
    foo1_new_name()
    foo2()
    foo3()
```

There is nothing called `foo1`!  This is a genuine semantic conflict.
Should the new name of `foo1` be `foo1_new_name` or
`foo1_another_new_name`?  The knowledge required to answer that
question is not contained within the conflict markers of the merge
conflict.  Local reasoning can't help.  You now have to think globally
about the meaning of the two branches in question.

## Conclusion

Resolving rebase conflicts is straightforward as long as you have a
clear idea of what the logically-intended changes introduced by the
rebased commit are, and you can see the state of base branch that you
need to apply them to---and there is semantic compatibility between
the two!

The git tooling can be used to provide useful information.  It's not
ideal though.  I explore a better conflict marker format in [Better
display for git rebase conflicts](../git-rebase-conflicts-display/).

## References

I found David Winterbottom's [article on the same
topic](https://codeinthehole.com/guides/resolving-conflicts-during-a-git-rebase/)
helpful when writing this one.

* [A Stackoverflow answer](https://stackoverflow.com/a/7589612) with
 compatible information

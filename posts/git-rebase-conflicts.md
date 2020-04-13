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

1. Issue the `git rebase` command.

   Use the `diff3` conflict style option, for example

   ```
   git -c merge.conflictStyle=diff3 rebase ...
   ```

   because it shows important
   information in the conflict markers that would otherwise be absent.

2. For each conflict, observe the logical change that the rebased
   commit was trying to make.

   * Either look at the difference between the middle hunk and the bottom
     hunk of the marked conflict, or

   * look at `git show REBASE_HEAD`

3. Observe the state of the base branch at each conflict region.

   * Either look at the top hunk of the marked conflict, or

   * look at `git show HEAD:<filename>`

4. Apply, to the state of the base branch, the logical change that the
   rebased commit was trying to make.

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

* The output of `git show REBASE_HEAD`.  This is generally easier to
  interpret than the above, but more verbose as it also contains diffs
  relating to non-conflicting parts of the patch.  In this case it
  shows

```diff
     foo1()
     foo2()
     foo3()
+    foo5()
```

Using either method you can see that the logical change of the commit
you are rebasing is to add `foo5()` after `foo3()`.

### Resolving the conflict

There are two equivalent ways to see the state of the base branch.

* The top hunk (below `HEAD`)

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

The correct way to resolve this conflict is to apply the logical
change of the rebased commit---adding `foo5()` after `foo3()`---to
state of the base branch---which has `foo4()` after `foo3()`.  In your
editor this will typically be easiest to do by making the change to
the top hunk and then deleting the other hunks.  It requires semantic
understanding to know exactly which way of resolving the conflict is
satisfactory, if any.  For example, we could put `foo5()` before or
after the appearance of `foo4()`.  Let's choose the latter and add
`foo5()` below `foo4()` in the top hunk (i.e. below `HEAD`).

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

and then delete the middle and bottom hunk, and the conflict markers,
to get

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

By looking at the difference between the "merged common ancestors"
hunk and the rebased patch hunk, or by looking at `git show
REBASE_HEAD`---which shows

```diff
 def main():
     foo1()
     foo2()
-    foo3()
```

---we can see that the intent of the rebased patch was to remove
`foo3()`.  On the other hand, the hunk under `HEAD`, and `git show
HEAD:<filename>`---which is

```
def main():
    foo1()
    foo3()
```

---show that the base branch does not contain `foo2()`.  We need to
apply the logical intent of the rebased patch to this context, which
is sensibly done by removing the appearance of `foo3()` in the hunk
below `HEAD`

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

and then we delete the middle and bottom hunks and the conflict
resolution markers to get

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
in the top hunk (i.e. below `HEAD`) leads to

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

and after deleting the deleting the other hunks we are left with

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
remove `foo2()`.  Doing so in the top hunk gives

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

and after deleting the other hunks and conflict markers we are left
with

```
def main():
    foo1_new_name()
    foo3()
```

## Worked example: a renaming and an extraction

Suppose I rename `foo1` to `foo1_new_name` as above, but rebase on top
a commit which combines `foo1` and `foo2` into a new function
`foo1and2` like so

```
+def foo1and2():
+    foo1()
+    foo2()
+
 def foo1():
     print("foo1")

@@ -14,6 +18,5 @@ def foo5():
     print("foo5")

 def main():
-    foo1()
-    foo2()
+    foo1and2()
     foo3()
```


Then I get two different conflicting regions

```
++<<<<<<< HEAD
 +def foo1_new_name():
++||||||| merged common ancestors
++def foo1():
++=======
+ def foo1and2():
+     foo1()
+     foo2()
+
+ def foo1():
++>>>>>>> Replace foo1 and foo2 with foo1and2
      print("foo1")

...

  def main():
++<<<<<<< HEAD
 +    foo1_new_name()
 +    foo2()
++||||||| merged common ancestors
++    foo1()
++    foo2()
++=======
+     foo1and2()
++>>>>>>> Replace foo1 and foo2 with foo1and2
```

What was the logical intent of the rebasing commit?  `git show
REBASE_HEAD` shows

```
+def foo1and2():
+    foo1()
+    foo2()
+
 def foo1():
     print("foo1")

...

 def main():
-    foo1()
-    foo2()
+    foo1and2()
     foo3()
```

The logical intent at the first conflict region is to *add* a new
function `foo1and2` which calls `foo1` and `foo2`.  The logical intent
at the second conflict region is to *replace* the calls of `foo1` and
`foo2` with a call of `foo1and2`.  Applying these changes in the upper
hunks gives

```
++<<<<<<< HEAD
 +def foo1and2():
 +    foo1_new_name()
 +    foo2()
 +
 +def foo1_new_name():
++||||||| merged common ancestors
++def foo1():
++=======
+ def foo1and2():
+     foo1()
+     foo2()
+
+ def foo1():
++>>>>>>> Replace foo1 and foo2 with foo1and2
      print("foo1")

...

  def main():
++<<<<<<< HEAD
 +    foo1and2()
++||||||| merged common ancestors
++    foo1()
++    foo2()
++=======
+     foo1and2()
++>>>>>>> Replace foo1 and foo2 with foo1and2
```

leading to a final result of

```
def foo1and2():
    foo1_new_name()
    foo2()

def foo1_new_name():
    print("foo1")

...

def main():
    foo1and2()
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

 def foo2():
@@ -14,6 +14,6 @@ def foo5():
     print("foo5")

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
question is not contained within the conflict resolution markers of
the merge conflict.  Local reasoning can't help.  You now have to
think globally about the meaning of the two branches in question.

## References

<https://codeinthehole.com/guides/resolving-conflicts-during-a-git-rebase/>

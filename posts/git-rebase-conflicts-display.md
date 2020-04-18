# Better display for git rebase conflicts

In the article [Resolving git rebase
conflicts](../git-rebase-conflicts/) I explain a procedure for
resolving git rebase conflicts.  It requires understanding the logical
(semantic) intent of the rebased commit and applying it to the
current state of the base branch.

There are two ways we can better understand the logical intent of the
rebased commit.  Firstly, we could try looking at the (`diff3`-style)
conflict hunk.  For example it might look as follows.

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

The intent of the rebased commit is to transform the middle hunk
section into the bottom hunk section, in this case to remove `foo3()`.
Secondly, we could look at the output of `git show REBASE_HEAD`.  This
gives us a "unified diff" that may be easier to read.  It will look
something like the following.

```diff
 def main():
     foo1()
     foo2()
-    foo3()
```

The downside is that we will see *all* of the rebased commit, not just
the conflicting parts.  Can we combine the best of both?


Rebase conflict markers would be more helpful displayed in a different
form.  We need to see exactly two things: the diff of the rebased
commit at the conflict location and the context of the target branch.
From the examples in the prior article, where we had

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

it would have been better shown as

```diff
      foo1()
      foo2()
      foo3()
<<<<<<< HEAD
      foo4()
=======
+     foo5()
>>>>>>> Add foo 5
```

Where we had

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

it would have been better shown as

```diff
    foo1()
<<<<<<< HEAD
    foo3()
=======
    foo2()
-   foo3()
>>>>>>> Remove foo3
```

Where we had

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

it would have been better shown as

```
def main():
    foo1()
    foo2()
<<<<<<< HEAD
=======
    foo3()
+   foo4()
>>>>>>> Add foo4
```

Where we had

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

it would have been better shown as

```
def main():
<<<<<<< HEAD
    foo1_new_name()
    foo2()
=======
    foo1()
-   foo2()
>>>>>>> Remove foo2
    foo3()
```

Where we had

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

it would have been better shown as

```
def main():
<<<<<<< HEAD
    foo1and2()
=====
+   foo1_new_name()
-   foo1()
    foo2()
>>>>>>> Rename foo1 to foo1_new_name
    foo3()
```

Unfortunately we cannot patch this into git ourselves because [the
conflict style choices is hard-coded into
git](https://stackoverflow.com/a/50726972/997606).  Nonetheless, an
editor plugin or even text-to-text processor could rewrite the `diff3`
style into the "context plus diff style" I propose.  I have written [a
demo in
Haskell](https://github.com/tomjaguarpaw/ad/blob/master/hunk.hs).




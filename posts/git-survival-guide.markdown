# A Git survival guide

## Roughly orthogonal git commands

* To create a new branch from the current one

        git checkout -b <new branch>

* To switch to an existing branch

        git checkout <existing branch>

* To make a commit

        git -c interactive.singleKey=true commit --verbose --patch

* To revert an uncommited change

        git -c interactive.singleKey=true checkout --patch

* To add a new file

        git add --intent-to-add <filename>

  (or `-N`) I don't use `git add` because it adds the file to the staging area.
  I don't want to bother with the staging area (see below for more
  details).

* To see the difference between different revisions (or my working
  copy)

        git diff

  and it is useful to use `--word-diff` or `--ignore-whitespace`.

* To rebase

        git rebase

  or to be really specific about it

    git rebase -i --onto <target> <from> <to>

  The latter will interactively rebase the collection of patches
  `<from>..<to>` onto `<target>` (`<from>` is exclusive, `<to>`
  inclusive).  It's always a reasonable idea to rebase interactively
  so at least you can see the list of commits that will be rebased.
  It might turn out you got some of the arguments wrong and then you
  can bail out early.

* To split a patch in two

        # I'm currently on <branch>
  
        COMMIT=<commit>
        git checkout $COMMIT
        git reset $COMMIT^

        # Commit just the changes you want in the first patch
        git commit --patch --verbose

        git reset --hard HEAD
        git revert --no-edit HEAD
        git rebase --onto HEAD $COMMIT^ <branch>
        # ... which will always succeed because the revert returned the
        # working copy to the state it was in before $COMMIT was
        # applied.

        git rebase --interactive $COMMIT^
        # ... squash the third commit (the original one we wanted to
        # split) into the second (the Revert) and keep only the original
        # commit message.

* To see the entire history of my repository

        git log --graph --decorate --oneline --all

  If you only want to see the topology of the tree instead of every
  commit then add `--simplify-by-decoration`.  (You might like to read
  [Ian Miell's "git log -- The Good
  Parts"](https://zwischenzugs.com/2018/03/26/git-log-the-good-parts/).)

## Things I don't do

* I almost never `git merge`.  If I want to do a fast forward "merge"
  (which is not really a merge at all) then I do

        git merge --ff-only

  and if I want to bring my branch up to date with the remote then I
  do

    git rebase <remote>/<branch>

* I never `git pull`.  `git pull` just does `git fetch` followed by
  `git merge`.  I very rarely want to merge and when I do I will be
  explicit about it.  More likely I will do a `git fetch` followed by
  `git rebase origin/master`.

* I never stash for the long term.  Committing to a temporary branch
  is a much more robust solution but sometimes stashing is too
  convenient to not use.

    If you have "stashed" by committing then you can pop your "stash"
    by using `git cherry-pick` and doing `git reset HEAD^`.  You
    probably then want to delete the temporary branch.

* I never use the staging area.  It is a piece of mutable state and
  I'd rather remove it from my mental model.  You do not need to touch
  the staging area if you use `git commit --patch`.

* I never use `.gitignore`.  You don't need it if you use `git commit
  --patch`.

* I never `commit --amend`.  Interactive rebase (see above) allows you
  access to the same functionality without needing to remember a
  different command.

* I rarely use `cherry-pick`. I find that interactive rebase provides
  a better approach to reaching my goal.

## Advanced stuff

### All the things your branch has ever been

Are you scared that you've lost commits because of a rebase gone wrong
or some other git mystery?  Try this command with the name of your
branch in the place of `<branch>`.

```shell
BRANCH=<branch>; git log --graph --pretty=format:"%C(yellow)%h%Creset %cr: %s%d (Authored %ar)"   `git reflog $BRANCH | cut '-d ' -f1`
```

(Yes, it's long, and you should put it all on one line).  You will be
presented with a tree that contains everything that has even been on
the given branch.  You can then reset your branch to one of its
earlier versions and perhaps `cherry-pick` into it commits from other
versions.

## Explicit git

* `git checkout -b <new branch name> <existing revision>`

* `git push origin <local revision>:<name of remote branch>`

* `git fetch <url of remote repository> <branch>`

    Then you can do whatever you want with `FETCH_HEAD` and you will
    not have added any new remote.  For example, you can `git checkout
    -b <new branch name> FETCH_HEAD`.

## What a rebase/merge is

TODO: explain how conflicts mean you have to merge the semantic
content of *both* patches

## Darcs translation

* `darcs whatsnew` = `git diff`

* `darcs revert` = `git -c interactive.singlekey=true checkout --patch`

* `darcs rec` = `git commit --patch --verbose`

* `darcs pull` = `git fetch && git rebase origin/master && git push
  --force`

    The git version is somewhat different to `darcs pull` because
    instead of automatically merging on a pull we explicitly rebase on
    the target branch.  This is a bit saner and gets conflicts out of
    the way explicitly and sooner.

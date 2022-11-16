# A Git survival guide

## Roughly orthogonal git commands

* To see the current status of all branches in the repository

      git log --graph --decorate --all --oneline

  It is my most used git command.  I run it after almost every other
  git operation to maintain a clear understanding of the current
  repository state.

  For a slightly better display but much more complicated command line

      PAGER="less -S" \
        git log \
          --graph \
          --decorate \
          --all \
          --pretty=format:"%C(auto)%h %<(7,trunc)%C(auto)%ae%Creset%C(auto)%d %s [%ar]%Creset"

  If you only want to see the topology of the tree instead of every
  commit then add `--simplify-by-decoration`.  (You might like to read
  [Ian Miell's "git log -- The Good
  Parts"](https://zwischenzugs.com/2018/03/26/git-log-the-good-parts/).)

* To see the difference between different revisions (or my working
  copy)

      git diff --color-moved

  and it is useful to use `--word-diff` or `--ignore-whitespace`.

  This is my second most used git command.

* To make a commit

      git commit --verbose --patch

  (See [`interactive.singleKey=true`](#config-interactive-singlekey))

* To see the full history of my current branch

  ```
  git log --patch --color-moved
  ```

* To revert an uncommited change

      git checkout --patch

  (See [`interactive.singleKey=true`](#config-interactive-singlekey))

* To create a new branch from the current one

      git checkout -b <new branch>

* To switch to an existing branch

      git checkout <existing branch>

* To add a new file

      git add --intent-to-add <filename>

  (or `-N`). I don't use `git add` because it adds the file to the staging area.
  [I don't want to bother with the staging area](#no-staging-area).

* To rebase

      git rebase ...

  (See [`merge.conflictStyle=diff3`](#config-merge-conflict-style))

  To be really specific about which sequences of commits you are
  rebasing where, use

      git rebase -i --onto <target> <from> <to>

  The latter will interactively rebase the collection of patches
  `<from>..<to>` onto `<target>` (`<from>` is exclusive, `<to>`
  inclusive).  It's always a reasonable idea to rebase interactively
  so at least you can see the list of commits that will be rebased.
  It might turn out you got some of the arguments wrong and then you
  can bail out early (by deleting all the text from the interactive
  rebase instruction file, saving it, and closing the editor).

* To split a commit in two use [my `git-split.sh`
  script](https://raw.githubusercontent.com/tomjaguarpaw/ad/300d63ae15ed2f12f48f83883af1aacad6905ddf/git-split/split.sh).

      sh <path>/git-split.sh bash <commit-id-to-split>

  There are many, many guides on the web about "how to split a git
  commit".  All the ones that I have seen give some version of the
  ["Splitting Commits" recipe from the `git rebase` man
  page](https://git-scm.com/docs/git-rebase#_splitting_commits).  That
  recipe is fiddly for a non-expert and one can easily find oneself in
  a confused state.  On the other hand the recipe can easily be made
  robust and packaged in [a script that gives the user helpful hints
  and can't
  fail](https://raw.githubusercontent.com/tomjaguarpaw/ad/300d63ae15ed2f12f48f83883af1aacad6905ddf/git-split/split.sh)
  -- so that's what I did!

* Inform my local repository of the state of the remote

  ```
  git fetch
  ```

  Then I can refer to remote branches via the names
  `<remote>/<branch-name>`, for example `origin/master`.

* To see the status

      git status -uno

  By default `git status` shows untracked, unignored files.  I don't
  need to ignore files because I use `git commit --patch`.  On the
  other hand I don't want to see untracked, unignored files in `git
  status`.  Instead I use `git status -uno` which supresses display of
  untracked files.

* grep the repository, without looking at untracked files or the
  `.git` directory

  ```
  git grep ...
  ```

## Global config settings

* <a name="config-interactive-singlekey"></a>To avoid having to press `<Enter>`
  after each choice

      git config --global interactive.singlekey true

  (in `git commit --patch`, `git checkout --patch`, `git stash
  --patch`, etc.)  `interactive.singleKey=true` requires the Perl
  module `Term::ReadKey`.  On Debian this is available in the
  `libterm-readkey-perl` package.

* <a name="config-merge-conflict-style"></a>To be able to resolve
  rebase/merge conflicts

      git config --global merge.conflictStyle diff3

  [It is literally impossible](../git-rebase-conflicts/) to resolve
  rebase/merge conflicts without using the `diff3` conflict style.

## Things I don't do

* <a name="no-staging-area"></a>
  I never use the staging area.  It is a piece of mutable state and
  I'd rather remove it from my mental model.  You do not need to touch
  the staging area if you use `git commit --patch`.  (Apparently
  [empirical research](https://news.ycombinator.com/item?id=33614904)
  has validated the hypothesis that the staging area is a major pain
  point for users in practice.)


* I never use `.gitignore`.  You don't need it if you use `git commit
  --patch`.

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

* I never `commit --amend`.  Interactive rebase (see above) allows you
  access to the same functionality without needing to remember a
  different command.

* I rarely use `cherry-pick`. I find that interactive rebase provides
  a better approach to reaching my goal.

## Explicit git

Many git commands make it possible to perform an action on a commit or
on a remote without checking out that commit or without adding that
remote.

* Show a file from a commit without checking that commit out

  ```
  git show <commit>:<filename>
  ```

* Create a new branch at an existing revision without checking out
  that revision first

  ```
  git checkout -b <new branch name> <existing revision>
  ```

* Push a branch to the remote without explicitly adding that remote or
  explicitly adding a remote tracking branch

  ```
  git push <url of remote repository> <local revision>:refs/heads/<name of remote branch>
  ```

   The `refs/heads` prefix is only necessary if the branch doesn't
   already exist on the remote.  If the branch does exist then the
   prefix is redundant but doesn't do any harm.

   Instead of `<url of remote repository>` you can use a
   previously-defined remote name, like `origin`, if you want.

* Fetch a branch from a remote without explicitly adding that remote

  ```
  git fetch <url of remote repository> <branch>
  ```

    Then you can do whatever you want with `FETCH_HEAD` and you will
    not have added any new remote.  For example, you can `git checkout
    -b <new branch name> FETCH_HEAD`.

   As above, instead of `<url of remote repository>` you can use a
   previously-defined remote name, like `origin`, if you want.

## What a rebase/merge conflict is

When you are presented with a rebase or merge conflict your job is to
merge the semantic content of *both* patches.  In particular, you
should not be choosing just "ours" or just "theirs".  See [Resolving
git rebase conflicts](../git-rebase-conflicts) for more information.

## Advanced stuff

### All the things your branch has ever been

Are you scared that you've lost commits because of a rebase gone wrong
or some other git mystery?  Try this command with the name of your
branch in the place of `<branch>`.

```shell
BRANCH=<branch>; \
PAGER="less -S" \
git log --graph \
        --decorate \
        --pretty=format:"%C(auto)%h %<(7,trunc)%C(auto)%ae%Creset%C(auto)%d %s [%ar]%Creset" \
        $(git reflog $BRANCH | cut '-d ' -f1)
```

You will be
presented with a tree that contains everything that has even been on
the given branch.  You can then reset your branch to one of its
earlier versions and perhaps `cherry-pick` into it commits from other
versions.

### All the changes to files you care about

```shell
git fetch && PAGER="less '+/^commit'" git log --patch --since="1 week" --extended-regexp --author='^([^T]|.[^o])' -- $(git rev-list --since="1 month" --author="Tom Ellis" origin/master | while read commit; do git diff-tree --no-commit-id --name-only -r $commit; done)
```

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

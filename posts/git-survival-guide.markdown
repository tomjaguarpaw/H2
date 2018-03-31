# A Git survival guide for Darcs users

Draft

* `darcs whatsnew` = `git diff`

* `darcs revert` = `git -c interactive.singlekey=true checkout --patch`

* `darcs rec` = `git commit --patch --verbose`

* `darcs pull` = `git fetch && git rebase origin/master && git push
  --force`

    The git version is somewhat different to `darcs pull` because
    instead of automatically merging on a pull we explicitly rebase on
    the target branch.  This is a bit saner and gets conflicts out of
    the way explicitly and sooner.

* Never `git merge`

* Never `git pull` (because `pull` implies a merge).  Instead `git
   fetch` and carefully rebase on the remote branch.

* Never stash, just commit to a temporary branch.  When you want to
  come back to your "stash" checkout the temporary branch and `git
  reset HEAD^`.

* Never use the staging area.  It's a piece of mutable state you just
  don't need if you use `git add --patch`.

* Never use `.gitignore`.  It's another piece of mutable state you
  just don't need if you use use `git commit --patch`.

## Git log

There are some good suggestions here
https://zwischenzugs.com/2018/03/26/git-log-the-good-parts/

* `git log --graph --all  --oneline --decorate --simplify-by-decoration`

* `git log --graph --all  --oneline --decorate`

## Explicit git

* `git checkout -b new_branch existing_commit`

* `git push origin <local revision>:<name of remote branch>`

## Roughly orthogonal git commands

* `git -c interactive.singleKey=true commit --verbose --patch`
* `git -c interactive.singleKey=true checkout --patch`
* `git add --intent-to-add`
* `git diff --word-diff`
* `git diff --ignore-whitespace`
* `git rebase --onto`
* `git rebase --interactive`
* `git log --graph --decorate --oneline --all`

## What a rebase/merge is

TODO: explain how conflicts mean you have to merge the semantic
content of *both* patches

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


## Explicit git

* `git checkout -b new_branch existing_commit`

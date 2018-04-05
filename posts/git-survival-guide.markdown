# A Git survival guide for Darcs users

Draft

*                 git diff = darcs whatsnew

*                 git -c interactive.singlekey=true checkout --patch

*                 git commit --patch --verbose

*                 git fetch && git rebase origin/master && git push --force

*                 NEVER git merge

*                 NEVER git pull (because that implies a merge)

*                 NEVER stash, just commit to a temporary branch and rebase

* NEVER use the staging area.  It's a piece of mutable state you just
  don't need if you use `git add --patch`.

* NEVER use .gitignore.  It's another piece of mutable state you just
  don't need if you use use `git commit --patch`.

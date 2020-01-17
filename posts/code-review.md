# Code review

Code review is for many things.  Link to Wikipedia article for
Overdetermination.


I prefer revieving a sequence of small, independent PRs.  However, the
number of comments that a PR attracts tends to be inversely
proportional to its size and complexity.


What to not do on a code review

* Comment about things that this PR does not make worse

  If this PR is touches a hairy area of the codebase that should be
  improved *but does not make it worse* please resist the temptation
  to suggest "while you're here ..." or "I've just noticed that ...".
  By all means write up tickets for future work, but the discussion
  doesn't belong on *this* PR.

* Make some comments but then leave the PR author's responses hanging.

## Links

The best code review guide I know about is [Google's Code Review
Developer Guide](https://github.com/google/eng-practices).

* "One business day is the maximum time it should take to respond to a
code review request"

[Modern Code Review: A Case Study at
Google](https://sback.it/publications/icse2018seip.pdf)

* the main impetus behind the introduction of code review was to force
  developers to write code that other developers could understand"

* "At Google, over 35% of the changes under consideration modify only
a single file and about 90% modify fewer than 10 files. Over 10% of
changes modify only a single line of code, and the median number of
lines modified is 24"

* "At Google ... fewer than 25% of changes have more than one
reviewer,1 and over 99% have at most five reviewers with a median
reviewer count of 1"

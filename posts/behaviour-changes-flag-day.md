# Behaviour changes on flag day

At work ([Groq](https://groq.com/)) we recently upgraded the version
of GHC that we use from 8.10 to 9.6, along with many of the Haskell
packages we depend on.  I wrote about the changes to our code that
this required in "[Upgrading from GHC 8.10 to GHC 9.6 -- an experience
report](../ghc-8.10-9.6-experience-report)".  That article touches on
the risks associated with being forced to make many changes at once:
if a problem occurs it can be hard to diagnose and it can be hard to
fix.  After that article was written I discovered that such a problem,
albeit a benign one, had occurred.  The present article explains the
situation.

We use [Nix](https://en.wikipedia.org/wiki/Nix_(package_manager))'s
`nixpkgs` to provide GHC and Haskell packages.  Consequently, we
upgraded GHC and our Haskell packages at exactly the same time by
upgrading to a later version of `nixpkgs`, specifically all in the
same commit to our source code repository.  I'm not familiar with Nix
or `nixpkgs` so I can't explain exactly why we followed the approach
of upgrading GHC and all packages at the same time, whether it was
literally forced on us by how Nix and `nixpkgs` work, or whether
upgrading everything at the same time was simply the most
straightforward of a variety of different options.  Regardless, that
means we experienced a "flag day".  Paraphrasing [Wikipedia's
definition and
elaboration](https://en.wikipedia.org/wiki/Flag_day_(computing)):

> A flag day is a change which requires a complete conversion of a
> sizable body of software. The change is large and expensive, and—in
> the event of failure—similarly difficult and expensive to reverse.
>
> The situation may arise if there are limitations on backward
> compatibility and forward compatibility among system components,
> which then requires that updates be performed almost simultaneously
> (during a "flag day cutover"). This contrasts with the method of
> gradually phased-in upgrades, which avoids the disruption of service
> caused by en masse upgrades.

In the language of [my previous
article](ghc-8.10-9.6-experience-report#forward-compatible-mitigations-versus-breaking-fixes),
a "flag day cutover" is required when incorporating a large number of
"breaking fixes".  By contrast, "forward compatible mitigations" can
be made as "gradually phased-in upgrades".

We experienced a "failure" of a sort after our flag day, albeit a
benign one, specifically the failure of a [golden
test](https://en.wikipedia.org/wiki/Characterization_test), that is, a
test that checks for exact equality of the output of a particular
function or program.  Golden tests often over-specify behaviour and
that was the case here too: the output artifact under test was a
program for Groq's LPU[^1], and although the binary contents of the
program file differed before and after flag day, it produced the same
results, so the test was too strict.

So, although there was a test failure, it was not actually indicative
of a bug.  There was an easy fix: update the golden test.  But I was
curious: what caused the difference in behaviour?  It was very
difficult to determine because the behaviour change occurred across a
flag day boundary!  If we had been able to upgrade GHC and each
package independently we could have made those upgrades as separate
commits to our repository and subsequently I could have discovered
which package caused the test failure by bisecting through the
repository history.

On the other hand, the situation could have been much worse: if the
upgrade of one of our dependencies _had_ caused a bug then we would
have been in a difficult position!  There would be no way to test the
upgrades individually to determine which caused it (or indeed, which
of our breaking fixes accidentally introduced it).

My impression is that upgrading a compiler and dependencies using
predetermined package snapshots where all packages change at once,
whilst an extremely convenient way of avoiding dependency conflicts,
is very awkward when it forces you to incur a flag day.  Is there
perhaps a way of getting the best of both worlds: a predetermined
package snapshot that contain _two_ versions of each package, that can
be upgraded independently of each other? I would be interested to
know.

## Appendix: what was the cause?

It doesn't really matter what the exact cause was, because the change
was ultimately of no consequence.  However, I wanted to know for the
sake of my own interest.

Because I couldn't bisect through the repository history I resorted to
eyeballs and intuition.  After comparing the new output to the golden
test expected output I concluded that the difference must be due to a
different iteration order in a particular stage of our assembler
pipeline. Looking at the data structures involved in that stage, the
only one which I felt could undergo a change in iteration order was
[`HashMap`](https://hackage.haskell.org/package/unordered-containers-0.2.20/docs/Data-HashMap-Strict.html#t:HashMap).
The iteration order of `HashMap` is the numeric order of the hash of
the keys.  Had the
[`Hashable`](https://www.stackage.org/haddock/lts-22.33/hashable-1.4.4.0/Data-Hashable.html#t:Hashable)
instance of the key type changed?  Indeed yes. In [`hashable`'s change
log](https://github.com/haskell-unordered-containers/hashable/blob/6ef535fd0053427e85201903a894a4b3162e0229/CHANGES.md),
the version 1.4.3.0 entry contains "Change `hashInt` to mix bits
more", and our key hashes ultimately depended on `hashInt`.

To see this in practice, `hashWithSalt 1 (13 :: Int)` is `16777630`
under `hashable` version 1.3.0.0 (which we were using before flag day)
and `-6919028725695267684` under version 1.4.3.0 (which we are now
using after flag day).

To be clear: I am not blaming `hashable`.  Firstly, there was no bug
in the first place, rather the failure of an overly-strict golden
test.  Secondly, [`hashable` states very
clearly](https://hackage.haskell.org/package/hashable) that

> `Hashable` does not have a fixed standard. This allows it to improve
> over time.  Because it does not have a fixed standard, different
> computers or computers on different versions of the code will
> observe different hash values

I am not completely certain that this _is_ the cause of the issue
because I can't test the change to `hashable` in isolation!  But I am
fairly confident.

## Links

* [Opaleye's breakage
  policy](https://h2.jaguarpaw.co.uk/posts/opaleyes-api-breakage-policy/)

[^1]: Language Processing Unit

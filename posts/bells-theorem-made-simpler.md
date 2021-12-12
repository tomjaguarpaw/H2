# Bell's theorem made simpler

## Introduction

[Bell's theorem](https://en.wikipedia.org/wiki/Bell's_theorem) is a
mathematical result which proves that quantum physics is incompatible
with what are called "local hidden-variable theories".  [John Stewart
Bell](https://en.wikipedia.org/wiki/John_Stewart_Bell) came up with a
mathematical inequality that "local hidden-variable theories" must
satisfy, yet it was subsequently demonstrated by experiment that
quantum systems can violate the inequality.  One philosophical
interpretation is that the physical world is non-local: actions in one
part of the physical universe can *instantaneously* affect other parts
of the universe.  Einstein called this "[spooky action at a
distance](https://en.wikipedia.org/wiki/Bell's_theorem#Historical_background)".

A first undergraduate course in quantum mechanics is enough to
understand Bell's theorem but nonetheless I found the standard
treatments completely incomprehensible, not mathematically
incomprehensible but incomprehensible in their meaning.  I struggled
to internalise the meaning of the inequality or why it should be
surprising that it could be violated.  This article presents the
treatment that I find most accessible.  I hope you find it as
fascinating as I do!

## The CHSH inequality

We will look at [the CHSH
inequality](https://en.wikipedia.org/wiki/CHSH_inequality#Statement)
because it is slightly more general than Bell's original inequality.
It states that if there are physical observables $a$, $b$, $a'$ and
$b'$, each of which can take on only values $-1$ or $+1$, then
\\[
|E(a, b) - E(a, b') + E(a', b) + E(a', b')|  \\leq 2
\\]
where each $E(x, y)$ is called the "quantum correlation" between an
observation $x$ and an observation $y$.  "Quantum correlation" sounds
mysterious but it's just the expectation of the product of $x$ and
$y$:
\\[
E(x, y) = \\mathbb{E}(xy)
\\]

It is of little benefit that the definition of "quantum correlation"
is technically simple.  I still have no intuition about it!  The CHSH
inequality is equally mysterious.  I have no intuition about whether
it is something that could be violated or not.  I have much more
intuition about probabilities than "quantum correlations".  Luckily
the "quantum correlation" can be rewritten simply in terms of
probabilities. After doing so the CHSH inequality will become very
natural!  We start by observing that, because $x$ and $y$ can take on
only values $+1$ and $-1$,
\\[
E(x, y)
= \\mathbb{E}(xy)
= \\mathbb{P}(x = y) - \\mathbb{P}(x \\ne y)
= 1 - 2\\mathbb{P}(x \\ne y)
\\]
so we can rewrite the CHSH inequality as
\\[
|1 - 2\\mathbb{P}(a \\ne b)
- 1 + 2\\mathbb{P}(a \\ne b')
+ 1 - 2\\mathbb{P}(a' \\ne b)
+ 1 - 2\\mathbb{P}(a' \\ne b')|
\\leq 2
\\]
<!--
thence
\\[
|- 2\\mathbb{P}(a \\ne b)
 + 2\\mathbb{P}(a \\ne b')
 - 2\\mathbb{P}(a' \\ne b)
- 2\\mathbb{P}(a' \\ne b') + 2|
\\leq 2
\\]
in particular
\\[
- 2\\mathbb{P}(a \\ne b)
 + 2\\mathbb{P}(a \\ne b')
 - 2\\mathbb{P}(a' \\ne b)
- 2\\mathbb{P}(a' \\ne b') + 2
\\leq 2
\\]
so -->
and after some algebra we conclude that
\\[
\\mathbb{P}(a \\ne b')
\\leq
\\mathbb{P}(a \\ne b)
+ \\mathbb{P}(a' \\ne b)
+ \\mathbb{P}(a' \\ne b')
\\]

Now *this* I can understand!  If $a \ne b'$ then at least one of $a
\ne b$, $a' \ne b$ and $a' \ne b'$ must hold.  This is elementary
probability theory of the most basic order. Of course it's true!

Is this form of the inequality sufficient to show that the predictions
of quantum mechanics differ from the predictions of basic probability
theory?  Remarkably yes.  There are simple experiments on quantum
systems that do *not* obey this inequality.

Specifically, we can perform the following.  Write
\\[
\\newcommand{\\bra}[1]{\\left\\langle{#1}\\right|}
\\newcommand{\\ket}[1]{\\left|{#1}\\right\\rangle}
\\newcommand{\\halfpi}{\\frac{\\pi}{2}}
\\ket{\\theta} =
\\left(\begin{array}{c}
\\cos\\theta\\\\\\sin\\theta\\
\\end{array}\\right)
\\]
for the state of a photon polarised at angle $\theta$.  A fundamental
physical property of these states is that they satisfy the
relationship
\\[
\\left\\langle \\theta_1 | \\theta_2 \\right\\rangle
= \\cos(\\theta_1 - \\theta_2)
\\]
The self-adjoint operator
\\[
S_{\\phi} =
\\ket{\\phi}\\bra{\\phi}
- \\ket{\\phi + \\halfpi}\\bra{\\phi + \\halfpi}
\\]
represents a measurement of the polarisation at angle $\phi$.
Specifically
\\[
S_\\phi \\ket{\\phi} = \\ket{\\phi}
\\;\\;\\;\\;\\;\\;\\;\\;
S_\\phi \\ket{\\phi + \\halfpi} = -\\ket{\\phi + \\halfpi}
\\]
That is, the value of an observation is $+1$ if the photon is indeed
polarised at angle $\phi$, and $-1$ if the photon is polarised at
angle $\phi + \pi/2$.  Now consider a pair of photons with entangled
polarisation
\\[
\\newcommand{\\V}{0}
\\newcommand{\\onesqrttwo}{\\frac{1}{\\sqrt{2}}}
\\newcommand{\\H}{\\halfpi}
\\psi
=
\\onesqrttwo
\\left(\\ket{\\V}\\ket{\\V}
+\\ket{\\H}\\ket{\\H}\\right)
\\]

Direct calculation shows that this state is, with respect to a general
orthonormal basis,
\\begin{multline}
\\psi
=
\\onesqrttwo\\cos(\\theta - \\theta') \\left(\\ket{\\theta} \\ket{\\theta'} + \\ket{\\theta + \\halfpi} \\ket{\\theta' + \\halfpi}\\right) \\\\
+ \\onesqrttwo\\sin(\\theta - \\theta') \\left(\\ket{\\theta} \\ket{\\theta' + \\halfpi} - \\ket{\\theta + \\halfpi} \\ket{\\theta'}\\right)
\\end{multline}
Therefore if we were to measure the first photon with $S_\theta$ and
the second photon with $S_{\theta'}$ then we would obtain the same
outcome (i.e. both $+1$ or both $-1$) in each case with probability
$\cos^2(\theta-\theta')$ and differing outcomes (i.e. one $+1$ and the
other $-1$) with probability $\sin^2(\theta-\theta')$.

Thus if we choose

* $a$ to be the observation of $S_0$
* $b$ to be the observation of $S_{\pi/8}$
* $a'$ to be the observation of $S_{\pi/4}$
* $b'$ to be the observation of $S_{3\pi/8}$

then
\\[
\\mathbb{P}(a \\ne b') = \\sin^2(3\\pi/8) = \\frac{2 + \\sqrt{2}}{4} > 3 / 4
\\]
but
\\[
\\mathbb{P}(a \\ne b) + \\mathbb{P}(a' \\ne b) + \\mathbb{P}(a' \\ne b')
= 3\\sin^2(\\pi/8) = 3 \\left(\\frac{2 - \\sqrt{2}}{4}\\right) < 3 / 4
\\]
so the CHSH inequality would be violated.  Indeed [this experiment has
been performed in
practice](https://en.wikipedia.org/wiki/Aspect%27s_experiment)!  There
is very strong evidence that physical reality is non-local.

## Interesting reading

[Quantum Mysteries for
Anyone](https://kantin.sabanciuniv.edu/sites/kantin.sabanciuniv.edu/files/makale/mermin.pdf)
by N. David Mermin presents a lucid introduction to the implications
of quantum physics on non-locality.  [Logical Bell
Inequalities](https://arxiv.org/abs/1203.1352) by Abramsky and Hardy
presents a general setting for understanding the difference between
the quantum world and the world of local realism.  Lucy Keer discusses
both of the above in in [Bell’s theorem and Mermin’s
machine](https://drossbucket.com/2020/09/12/bells-theorem-and-mermins-machine/).
[The Mermin-Peres magic square
game](https://en.wikipedia.org/wiki/Quantum_pseudo-telepathy#The_Mermin%E2%80%93Peres_magic_square_game)
presents the most remarkable violation of non-locality that I have
come across.


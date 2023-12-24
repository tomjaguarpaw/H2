# Is a vector space naturally isomorphic to its dual?

The answer is no: you know it, I know it, everyone who has spent years
becoming familiar with this family of spaces knows through intuition
that the answer is no: a vector space is not naturally isomorphic to
its dual.  But pinning down exactly *why* not proves challenging.

I [originally asked this
question](https://mathoverflow.net/questions/345136/is-a-vector-space-naturally-isomorphic-to-its-dual)
on MathOverflow on 3rd November 2019 but sadly the question was
closed.  I repeat it here verbatim before proceeding.

## The question

This question may not be as easy to answer as you think!  Some tangentially-related questions have appeared on math.stackexchange but I'm not really convinced by the answers.

In what follows I will assume all vector spaces under discussion are finite dimensional.

### A vector space is naturally isomorphic to its double dual

In an early linear algebra course  we are told that "a finite dimensional vector space is naturally isomorphic to its double dual".  The isomorphism in question is ${**}_V : V \to V^{**}$, $v^{**}(\phi) = \phi(v)$.  We are told that this isomorphism is "natural" because it doesn't depend on any arbitrary choices.  The notion of "natural", or "independent or arbitrary choice", is made precise via the concept of a category theoretical "natural transformation".  Specifically, the operation $**$ on vector spaces gives rise to a functor whose action on maps is $f^{**} : V^{**} \to W^{**}$, $f^{**}(v^{**}) = f(v)^{**}$.  In fact this is exactly the condition for the naturality square to commute and so ${**}_V$ is indeed a natural transformation (between the identity functor and $**$) which is an isomorphism.

### A vector space is naturally isomorphic to its dual!

So far, so familiar.  But there's something that doesn't quite hold up about all this.  Let's adapt the above to show that $V$ and $V^*$ are "naturally isomorphic".  We do this by following exactly the same procedure, replacing $**$ everywhere with $*$.  The only change we have to make is to come up with an arbitrary isomorphism $*_V$ for each $V$.  Other than that, the whole construction goes through unchanged.  Specifically, once we have chosen $*_V$ we define the functorial action on morphisms to be $f^{*} : V^{*} \to W^{*}$, $f^{*}(v^{*}) = f(v)^{*}$.

In particular I have a natural isomorphism between the identity functor and $*$!

### Objections to the construction

One could make a few objections to this construction, but they seem to be circular.

* "But you admitted that $*_V$ depends on an arbitrary choice!"

    I did, but that was informal language.  In what formal sense is it arbitrary?  The notion of "naturality" was supposed to rule out constructions that are arbitrary!

* "Your definition of $f^*$ is invalid.  It depends on $*_V$."

    So what?  My definition of $f^{**}$ depends on ${**}_V$ but it is uncontroversial.

    "You should have defined $f^{**}(\hat{v})(\phi) = \hat{v}(\phi \circ f)$ and then it's clear that it doesn't depend on $**_V$.  You can't do that for $f^*$."

    But your $f^{**}$ is the same as my $f^{**}$!  Is there some formal way of specifying that a functor does not depend on a natural transformation?  And besides, what's the problem if it does?

    "It's a problem because it depends on something that depends on arbitrary choice ..."

* "Whilst the functor $**$ is the real double dual functor, $*$ is one you just made up.  It is indeed isomorphic to the identity functor but that doesn't mean anything about 'a vector space being isomorphic to its dual'".

    Why not?  I've followed exactly the same recipe for both of them, using the notion of "natural transformation" as I was supposed to.

    "Sure, but the result is interesting only in the case of $**$ because your definition of $*$ depended upon arbitrary choice ..."

    And why is $**$ "the real" double dual functor?

    "Because its definition doesn't depend upon arbitrary choice ..."

* "Your definition of $*$ is not the real dual functor because it
  doesn't satisfy $* \circ * = **$"

  But you still haven't explained why $**$ *is* "the real" double dual
  functor.

### Conclusion

All attempts to explain why I haven't really shown that a finite dimensional vector space is naturally isomorphic to its dual seem to invoke circular reasoning.

I can only conclude that if the notion of natural transformation is going to be used to formalise the concept of "independent of arbitrary choice" then something needs to be tightened up.  My choice of $*_V$ was indeed arbitrary but it is not ruled out by the notion of natural transformation.

How could we proceed?  Could the absence of choice be used to rule out the construction of $*_V$?  [Answers to an earlier question seem to suggest that is an irrelevant issue](https://math.stackexchange.com/questions/49420/natural-isomorphisms-and-the-axiom-of-choice) but in light of the above I'm not convinced.  More generally, does this kind condition perhaps only make sense in a constructive or intuitionistic setting?  I have a clue about how to formalise this condition in type theory via parametricity, so perhaps that is the key!

(This question is similar to [an earlier one](https://math.stackexchange.com/q/3194724/13743).  I'm not convinced by the answer.  It seems to be making an objection of the third form above, which seems circular to me.)

## Proceeding to the answer

The aim of the question is twofold: firstly to demonstrate that there
is something fishy about the standard justification for the statement
that "a finite-dimensional vector space is *naturally isomorphic* to
its double dual" and secondly to ask for a less fishy definition of
"natural" that can be used to show whether or not there is a natural
mapping from a vector space to its dual (presumably there is not).

[Terminological note: when talking about the technical and specific
notions of *natural transformation* and *natural isomorphism* from
category theory I will use italics; when talking about the informal
and intuitive notion of "naturalness" I will use normal font.]

The statement that "a finite dimensional vector space is *naturally
isomorphic* to its double dual" appears in various places[^1].  We
have an intuitive sense that the isomorphism $v \mapsto \phi \mapsto
\phi(v)$ is natural but formally pinning down what "natural" means is
hard.  The standard category theoretical explanation in terms of a
*natural transformation* between functors offers one attempt.  Indeed
the statement holds first position in [Tom Leinster's
list](https://www.maths.ed.ac.uk/~tl/ct/zero.pdf) of "phenomena, facts
and concepts that category theory sheds light on".  But appealing to
*natural transformation* as an explanation of naturalness falls down
on two counts: it is too restrictive in one sense and insufficiently
restrictive in another.

The sense in which it is too restrictive is that many natural things
cannot be expressed as a *natural transformation* between functors at
all!  For example, suppose we want to know whether it is natural to
contract a vector with a dual vector $V \otimes V^* \to \mathbb{R}$.
The dual vector space functor is contravariant, so $V \mapsto V
\otimes V^*$ is not a functor at all and the notion of *natural
transformation* cannot shed light on this question!  But what
operation could be *more* natural than contraction?  If the concept of
*natural transformation* cannot speak about whether *contraction* is
natural then there must be a vast range of natural constructions that
it cannot speak about.  So, there are some candidates for naturality
which the concept of *natural transformation* gives no answer to.

By contrast, there is a sense in which appealing to *natural
transformations* to define naturalness is *insufficiently*
restrictive: there *is* a functor which on objects takes a space to
its dual space and is *naturally isomorphic* to the identity!  That
functor is the one described above in [my original MathOverflow
question above](#the-question).  Yet the components of the *natural
transformation* in question are not natural.  They are utterly
arbitrary!  We all feel it in our bones that there is nothing
"natural" about that transformation yet it *is* a *natural
transformation*.  So, there are some candidates for naturality which
category theory gives a false positive answer to.

## A elementary attempt at an answer

The former difficulty, the one in which the notion of *natural
transformation* has insufficient expressivity, is the first hurdle
that we encounter when asking whether "a vector space is *naturally
isomorphic* to its dual".  To reiterate, the usual dual vector space
functor is contravariant, so it doesn't even make sense to ask whether
it is *naturally isomorphic* to the identity functor.  On the other
hand, this difficulty can be finessed.


<a name="axioms"></a>Here is a elementary route to the answer.  Let us
take as axiomatic that the following operations are natural and that
the composition of natural maps is natural[^2].

* contraction: $v \otimes \phi \mapsto \phi(v)$
* applying a natural map $g$ to one component of a tensor product: $v
  \otimes w \mapsto v \otimes g(w)$

Next let $g$ be an arbitrary natural map $V \to V^*$.  Our goal is to
determine what $g$ could possibly be.  We start by noticing that given
an element of $V \otimes V$ we can apply $g$ to the right component to
obtain an element of $V \otimes V^*$. Subsequently we can contract to
obtain an element of $\mathbb{R}$.  If our hypothesis holds then we
have produced a natural linear map $b : V \otimes V \to \mathbb{R}$ or
in other words, a natural bilinear form on $V$.  Our intuition is even
stronger about this that it was about dual spaces: vector spaces do
not come with a natural non-zero bilinear form!

This simple change of perspective brings the question into a covariant
form: both $V \otimes V$ and $\mathbb{R}$ are functors[^3] covariant
in $V$.  Therefore we can ask whether $b$ is a natural transformation
$V \otimes V \to \mathbb{R}$.  If so its naturality square must
commute, that is $b_{V'}(f(v) \otimes f(v')) = b_{V}(v \otimes v')$.
We deduce that $b$ is zero by simply choosing $f = 0$, hence $g : V
\to V^{*}$ is zero.

If we accept (more on this later) that category theory never gives
false negative answers to questions of naturalness then we have our
answer: there is no non-zero natural bilinear form and therefore no
non-zero natural map $V \to V^*$.

Hidden in this approach is the answer to why my earlier attempt to
construct of a natural map $V \to V^*$ should be ruled out.
Specifically, it shows why my definition of $*$ is not "the real" dual
space functor: it does not respect the structure of $V^*$ as a
function space because under it contraction $V \otimes V^* \to
\mathbb{R}$ is not natural.  The concept of *natural transformation*
cannot directly show the non-naturalness of my construction because it
cannot be applied to contraction.  It was only by taking contraction
as axiomatically natural that we were able to change our perspective
to one that the concept of *natural transformation* could talk about.

That is, we can answer the following questions

* *"Why are the standard double dual functor $**$ that we know and love,
  along with its counterpart, the contravariant dual functor $*$, 'the
  real' double dual and dual functors respectively?"*

  Because they respect the structures of $V^{**}$ and $V^*$ as
  function spaces, i.e. their naturality is consistent with
  contraction $V \otimes V^{*} \to \mathbb{R}$ being natural.  This is
  a concrete mathematical fact that can be verified without any
  informal appeal to the intuitive lack of arbitrariness in their
  definitions.

* *"Why is appealing to natural transformations to define naturalness
  insufficiently restrictive?"*

  Because there are other conditions that must be placed on the
  functors in question for naturalness to be meaningful.  Such a
  condition is respect for function space structure.

* *"Why is my proposed definition of a contravariant dual space functor
  not a candidate for 'the real' such functor?"*

  Because it does not respect the structure of $V^{*}$ as a function
  space, i.e. its naturality is not consistent with contraction $V
  \otimes V^{*} \to \mathbb{R}$ being natural.  Again, this property
  can be formally verified.  It is not an informal condition that we
  have to use our intuition about.

* *"What is fishy about the standard justification for the claim that 'a
  vector space is naturally isomorphic to its double dual'?"*

  Because it doesn't make explicit the vital additional property that
  the action of $**$ on morphisms respects the structure of $V^{**}$
  as a function space.

* *"Why does my construction not show that 'a vector space is naturally
  isomorphic to its dual'?"*

  Because my construction doesn't satisfy the implicit condition of
  respecting the structure of the dual as a function space.

### Naturality of contraction

We took as axiomatic that the following operations are natural.  Can
category theory provide us with justification for that assumption?

* contraction: $v \otimes \phi \mapsto \phi(v)$
* applying a natural map $g$ to one component of a tensor product: $v
  \otimes w \mapsto v \otimes g(w)$

Let's look at contraction.  The category of vector spaces is *closed
monoidal*, that is, its tensor operation $\otimes$ and its linear map
former $\multimap$ satisfy

\\[
\\textrm{Hom}(A, B \\multimap C) \\cong \\textrm{Hom}(A \\otimes B, C)
\\]

where this bijection (which arises from a particular adjunction) is
natural in $A$ and $C$[^4].  Let's look at what happens if we choose
$A = B \multimap \mathbb{R}$, and $C = \mathbb{R}$.  The bijection
becomes

\\[
\\textrm{Hom}(B \\multimap \\mathbb{R}, B \\multimap \\mathbb{R}) \\cong
\\textrm{Hom}((B \\multimap \\mathbb{R}) \\otimes B, \\mathbb{R})
\\]

or, since $B \multimap \mathbb{R}$ is typically written $B^*$,

\\[
\\textrm{Hom}(B^{\*}, B^{\*}) \\cong
\\textrm{Hom}(B^{\*} \\otimes B, \\mathbb{R})
\\]

Now, applying a natural bijection to a natural element ought to result
in another natural element, so if we hypothesise that $\mathrm{id} \in
\mathrm{Hom}(B^*, B^*)$ is natural, then we conclude that its image in
$\mathrm{Hom}(B^{*} \otimes B, \mathbb{R})$ is natural.  That image is
contraction, thus contraction is natural!  We can replace our axiom
about naturality of contraction with a simpler axioms about the
naturality of $\mathrm{id}$ and the naturality of bijections arising
from adjunctions.

Presumably a similar line of reasoning can conclude that
$v \otimes w \mapsto v \otimes g(w)$ is natural, based on the
naturality properties of monoidal categories.

## Remaining questions

* We have not yet demonstrated a complete theory of
  naturalness. Although we can we do better than take [contraction and
  other operations as axiomatically natural](#axioms) we still haven't
  provided a complete axiomatisation of naturality. Is there a setting
  in which we can directly state what it means for those operations to
  be natural, rather than having to shift them into a covariant form
  in which we can apply the notion of *natural transformation*?

* Can we explain why category theory never gives false negative
  answers to questions of naturalness in the contexts that it can
  answer, despite not being able to answer in all contexts and despite
  giving false positives in some contexts in which it can answer?

[^1]: for example in [Jason Polak's
    writing](https://blog.jpolak.org/?p=1100)

[^2]: If they were not natural then we would be surprised if
*anything* was!

[^3]: As a functor $\mathbb{R}$ is the zero-fold tensor product of
$V$.  It takes all morphisms $f : V \to V'$ to the identity function
$I : \mathbb{R} \to \mathbb{R}$.

[^4]: It would probably be required to be natural in $B$ too, if it
    could be!

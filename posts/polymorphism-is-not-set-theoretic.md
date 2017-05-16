# Polymorphism is not set theoretic

In [Polymorphism is not
set-theoretic](https://hal.inria.fr/inria-00076261/document), John Reynolds
demonstrates that polymorphic lambda calculus has no set theoretic models,
or, more precisely, no set theoretic models which preserve the binary
product and function space and also satisfy some weak condition on universal
quantification.  The proof was elucidated in [On Functors Expressible in the
Polymorphic Typed Lambda
Calculus](http://homepages.inf.ed.ac.uk/gdp/publications/Functors_Expressible_Polymorphic.pdf)
by John Reynolds and Gordon Plotkin.  The exposition in the latter is much
clearer.  I'll reproduce a rough sketch of the proof here.

The crux of the argument is to show that if a set-theoretic model existed
then it would contain all initial algebras of "definable functors", i.e. 
all initial algebras of those functors on Set which are definable within the
polymorphic ambda calculus.  Then a particular initial algebra can be
constructed whose existence is a contradiction.

Roughly, the proof proceeds by defining `P = Fix T = forall k.  (T k -> k)
-> k` for any lambda calculus functor `T`.  This comes with a special map `h
:: T P -> P`.  (In some sense this is an initial algebra in the lambda
calculus but without reference to a denotational semantics perhaps that is a
meaningless concept.)  In any case, assuming we have a set theoretic model,
the denotations of `T` and `h` give a weak initial algebra on Set.  Now in
Set all weak initial algebras are in fact initial algebras, because certain
equalizers exist.  Thus `T` and `h` give moreover an initial algebra and
hence, in denotation, `T P` is isomorphic to `P`.

To arrive at a contradiction we simply choose `T a = ((a -> Bool) -> Bool)`. 
Then the denotation of `T P` cannot be isomorphic to the denotation of `P`
since it has larger cardinality.

## Further questions

* What on earth *is* `Fix ((_ -> Bool) -> Bool)`?

    * [Andrej
      Bauer has an interesting post](http://math.andrej.com/2009/10/12/constructive-gem-double-exponentials/)
      identifing it with `Nat` but it's not yet clear to me whether the
      identification depends on adding specific axioms to constructive set
      theory.

* Is it morally the same thing in all models?

* If it is different in different models what are the philosophical
  implications?

* Is it useful for programming?

* If not, is it possible to construct a polymorphic lambda calculus that has
  enough polymorphism for programming but not so much polymorphism that it
  permits the existence of `Fix T`?


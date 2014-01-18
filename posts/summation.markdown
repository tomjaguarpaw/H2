# Summation

[Numberphile's video on summing the positive
integers](https://www.youtube.com/watch?v=w-I6XTVZXww) has been
confusing people, because physicists have a way of working with
mathematics that is very foreign to mathematicians.  Let's see what
Numberphile's proposed proof *can* say about the sum of the positive
integers that is mathematically rigorous.

We've been given

* S = 1 + 2 + 3 + 4 + 5 + ...
* S_2 = 1 - 2 + 3 - 4 + 5 - ...

We can generalise this slightly

* S(x) = 1 + 2x + 3x^2 + 4x^3 + 5x^4 + ...
* S_2(x) = 1 - 2x + 3x^2 - 4x^3 + 5x^4 - ...

and for full generality we will treat these as *formal* power series.
I'm not going to consider the functions they induce until right at the
end.

Now, it's perfectly mathematically rigorous to say that S_2(x) = 1 /
(1 + x)^2 as formal power series (indeed it's true when they are
considered as functions on -1 < x < 1, but hold your horses!).

It's also perfectly mathematically rigorous to say that

* S(x) - S_2(x) = 4x + 8x^2 + 12x^5 + ... = 4x S(x^2)

so that

* S_2(x) = S(x) - 4x S(x^2)

which when the right hand side is expanded is

* S_2(x) = (1 - 4x) + 2x(1-4x^2) + 3x^2(1 - 4x^3) + 4x^3(1 - 4x^4) + ...

Note that in particular this is *not* S_2(x) = -3 S(x)!  Now observe
that if we consider the functions on **R** induced by these power
series we do indeed find that

* 1/4 = lim (x < 1) { (1 - 4x) + 2x(1-4x^2) + 3x^2(1 - 4x^3) + 4x^3(1 - 4x^4) + ... }

This is about as close as we can get, I think, using a rigourous form
of Numberphile's proof, to the physicists claim that

* 1/4 = -3 (1 + 2 + 3 + 4 + 5 + ...)

## Decategorification

I have no doubt that when physicists use their identity, what they are
doing is completely valid.  What's going on here is analogous to
[decategorification](http://ncatlab.org/nlab/show/decategorification),
that is, forgetting some important part of the structure.  The
important part of the structure here is the formal variable x that we
hang coefficients onto to express the sum.

In the contexts that physicists use this identity, I assume that there
is some other underlying structure which "decategorifies" to this sum
of integers (perhaps it really is as simple as having this formal
variable x).  What's wrong here mathematically is not the result but
stating it and proving it in the "decategorified" setting, when the
statement and proof are only valid in the original setting, not
after "decategorification".


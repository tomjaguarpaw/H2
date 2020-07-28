# Automatic differentiation: source-to-source worked examples

-- forwards and reverse

## Introduction

This article demonstrates how to perform source transformations on a
program to generate forward mode and reverse mode derivative programs
(automatic differentiation, or "AD").  My aim is to write the shortest
possible article that communicates all the essential features of a
source-to-source AD system with a particular focus on making the
reverse mode transformation clear.

The goal of brevity means that a lot of possible commentary has been
omitted.  If you find this makes some part of the article hard to
understand then please [contact
me](http://web.jaguarpaw.co.uk/~tom/contact) and I'll do my best to
clarify.  In particular this article contains hardly any mathematical
content at all.  I hope that the reader who is familiar with
multivariate calculus will be able to obtain an intuitive
understanding of how AD relates to mathematical techniques he or she
is already familiar with.  A more in-depth description of the
relationship will have to wait for another article.

## The program

Let's consider the following pseudocode program that performs some
elementary arithmetic through a sequence of assignment statements.

```
p = 7 * x
r = 1 / y
q = p * x * 5
v = 2 * p * q + 3 * r
```

`x` and `y` are not defined in the program so I'm going to informally
consider them to be "inputs"; `v` is not used anywhere so I'm going to
consider it to be the "output".  (I won't burden the article by
formalising these notions here.)

## Preparation

We'll do a small amount of preparation to our original program which
will preserve its behaviour and get it into a form in which it is
straightforward to apply the automatic differentiation (AD)
algorithms.  It is possible to apply AD algorithms without doing these
transformations first but then the AD algorithms would have to do
equivalent operations implicitly.  Doing these transformations first
is a kind of [separation of
concerns](https://en.wikipedia.org/wiki/Separation_of_concerns).

### Use prefix functions with exactly one argument

Let's use prefix functions instead of [infix
operators](https://en.wikipedia.org/wiki/Infix_notation).  Infix
operators are more familiar for arithmetic but the AD algorithms will
be clearer to present if we use prefix functions.  Additionally I want
every function to have exactly one argument (although that argument
may be a tuple).  Single-argument style will make the reverse mode
transformation much clearer (although it does not make any difference
for forward mode).  For example, `x1 + x2` would become `add (x1,
x2)`.  Our program becomes

```
p = mul (7, x)
r = div (1, y)
q = mul (mul (p, x), 5)
v = add (mul (mul (2, p), q), mul (3, r))
```

### No nested subexpressions

Next let's convert to a form where every function is applied to
(tuples of) variables and constants only, i.e. where there are no
nested sub-expressions (besides potentially nested tuples).  We assign
each nested sub-expression to an intermediate variable.  For example

```
a = add (add (b, c), d)
```

would become

```
i = add (b, c)
a = add (i, d)
```

The choice of `i` is arbitrary; it just has to be a variable that's
not used elsewhere in our program.  This form without nested
subexpressions is a lot like
[ANF](https://en.wikipedia.org/wiki/A-normal_form) from the field of
functional compiler construction.  It's also a lot like the [SSA
form](https://en.wikipedia.org/wiki/Static_single_assignment_form) of
assembly language.  After removing nested subexpressions, our program
becomes

```
p = mul (7, x)
r = div (1, y)
i1 = mul (p, x)
q = mul (i1, 5)
i2 = mul (2, p)
i3 = mul (i2, q)
i4 = mul (3, r)
v = add (i3, i4)
```

## Differentiation line-by-line

We have performed all the transformations needed to prepare our
program and we are ready to proceed to differentiation.  We will
differentiate the program line-by-line, that is, both the forward mode
and reverse mode differentiation algorithms will generate one line of
derivative code for each line of input code.  But what *is* the
derivative of an assignment statement?  For forward mode, the
derivatives correspond quite closely to what you might be familiar
with from a first multivariate calculus course..

### Examples

#### Addition

If a line of our pseudocode program is

```
y = add (x1, x2)
```

then the derivative line is

```
dy = add (dx1, dx2)
```

#### Multiplication

If a line of our pseudocode program is

```
y = mul (x1, x2)
```

then the derivative line is

```
dy = add (mul (x2, dx1), mul (x1, dx2))
```

#### Division

If a line of our pseudocode program is

```
y = div (x1, x2)
```

then the derivative line is

```
dy = add (div (dx1, x2), negate (mul (div (x1, mul (x2, x2)), dx2)))
```

## Forward mode

The forward mode transformation applies the appropriate
differentiation rule to each line in the input program (listed on the
left) to obtain a derivative line (listed on the right).  To each line
we apply exactly one rule and the form of the rule does not depend on
any of the other lines.

```
p = mul (7, x)   | dp = mul (7, dx)
r = div (1, y)   | dr = negate (div (dy, mul (y, y)))
i1 = mul (p, x)  | di1 = add (mul (dp, x), mul (p, dx))
q = mul (i1, 5)  | dq = mul (di1, 5)
i2 = mul (2, p)  | di2 = mul (2, dp)
i3 = mul (i2, q) | di3 = add (mul (di2, q), mul (i2, dq))
i4 = mul (3, r)  | di4 = mul (3, dr)
v = add (i3, i4) | dv = add (di3, di4)
```

If we form a new program consisting of the sequence of assignments on
the left followed by the sequence of assignments on the right then we
have a program that calculates the forward derivative!  The "inputs"
of this program are `x`, `y`, `dx` and `dy`.  The "outputs" are `v`
and `dv`.

(The derivatives of constants are zero and I've left terms that are
zero out for simplicity.)

In fact we can be a little more clever.  We can interleave the
assignments, so an assignment from the left is immediately followed by
its corresponding assignment from the right, that is

```
p = mul (7, x)
dp = mul (7, dx)
r = div (1, y)
dr = negate (div (dy, mul (y, y)))
i1 = mul (p, x)
di1 = add (mul (dp, x), mul (p, dx))
q = mul (i1, 5)
dq = mul (di1, 5)
i2 = mul (2, p)
di2 = mul (2, dp)
i3 = mul (i2, q)
di3 = add (mul (di2, q), mul (i2, dq))
i4 = mul (3, r)
di4 = mul (3, dr)
v = add (i3, i4)
dv = add (di3, di4)
```

This interleaving demonstrates an important property of the automatic
derivative: that it uses space proportional to the space usage of the
original program.  Specifically, as soon as we no longer need a
variable that was assigned in the original program we no longer need
the corresponding `d` version either.

We can also see another important property of the forward derivative:
it runs in time proportional to the run time of the original program
(assuming that the derivative of every primitive runs in time
proportional to the run time of the primitive itself).


## Reverse mode requires two additional ideas

Now that we've shown how to generate the forward mode derivative we
can move on to the reverse mode derivative.  Reverse mode requires two
additional ideas:

1. We need to convert our original program to "explicit duplication"
   form: if a variable is used more than once then we make that
   explicit in the structure of the program.  This is unusual but
   straightforward.

2. We need to use a form of the derivative that will be unfamiliar to
   most readers.  It will appear quite bizarre when seeing it for the
   first time but it is crucial to implementing the reverse mode
   derivative.

## Explicit duplication form

Before applying the reverse mode AD transformation we will convert to
"explicit duplication" form.  Again, the transformation is not
strictly required but if we omit it then the differentiation pass will
have to do it implicitly.  We take the ANF form of the program and
insert explicit duplications (`dup`) for any variable that is used
more that once.  Recall that after removing nested subexpressions our
program was

```
p = mul (7, x)
r = div (1, y)
i1 = mul (p, x)
q = mul (i1, 5)
i2 = mul (2, p)
i3 = mul (i2, q)
i4 = mul (3, r)
v = add (i3, i4)
```

We can see that `x` and `p` appear on the right hand side (i.e. are
consumed) twice each. Therefore, they will need explicit duplication,
so that each variable in the resulting program is used only once.
With explicit duplication the program looks like

```
(x1, x2) = dup x
p = mul (7, x1)
(p1, p2) = dup p
r = div (1, y)
i1 = mul (p1, x2)
q = mul (i1, 5)
i2 = mul (2, p2)
i3 = mul (i2, q)
i4 = mul (3, r)
v = add (i3, i4)
```

(If a variable were used $n$ times then we would have to insert $n-1$
`dup`s for it.  In our example no variable is used more than twice.)

Notice that now not only is every variable defined exactly once, but
every variable is also *used* exactly once (except the inputs and
outputs, `x`, `y` and `v` -- I won't say more here about how exactly
these seemingly special cases fit into the story).  This property is
important for a reason which will be explained when we come to
generate the reverse mode program.

## Differentiation line-by-line

The line-by-line differentiation rules for generating the reverse mode
need another article to explain thoroughly, but in this article I will
hope to provide some basic intuition via examples and the informal
notion that the reverse mode program calculates how sensitive the
output is to different variables.  For example, if the variable `y`
appears in the original program then the variable `d_dy` will appear
in the reverse mode program and measures "how sensitive the output is
to small changes in `y`".  (I'll abbreviate this to "`d_dy` is the
sensitivity to `y`".)

### Examples

#### Addition

If a line of our pseudocode program is

```
y = add (x1, x2)
```

then the derivatives are

```
d_dx1 = d_dy
d_dx2 = d_dy
```

because the sensitivity to `x1` is the same as the sensitivity to `y`
(and likewise for `x2`).  This is written on a single line as

```
(d_dx1, d_dx2) = dup (d_dy)
```

#### Multiplication

If a line of our program was

```
y = mul (x1, x2)
```

then the derivative line is

```
(d_dx1, d_dx2) = (mul (x2, d_dy), mul (x1, d_dy))
```

because the sensitivity to `x1` is `x2` times the sensitivity to `y`
(and similarly for `x2`).

#### Duplication

If a line of our program was

```
(x1, x2) = dup (x)
```

then the derivative line is

```
d_dx = add (d_dx1, d_dx2)
```

because the sensitivity to `x` is the sensitivity to `x1` plus the
sensitivity to `x2`.

## Generating reverse mode code

Like forward mode before it, the reverse mode transformation applies the
appropriate differentiation rule to each line in the input program
(listed on the left) to obtain a derivative line (listed on the
right).  To each line we apply exactly one rule and the form of the
rule does not depend on any of the other lines.


```
(x1, x2) = dup x  | d_dx = add (d_x1, d_dx2)
p = mul (7, x1)   | (_, d_dx) = mul (d_dp, (x1, 7))
(p1, p2) = dup p  | d_dp = add (d_dp1, d_dp2)
r = div (1, y)    | d_dy = negate (div (d_dr, mul (y, y)))
i1 = mul (p1, x2) | (d_dp1, d_dx1) = mul (d_di1, (x, p1))
q = mul (i1, 5)   | (d_di1, _) = mul (d_dq, (5, di1)
i2 = mul (2, p2)  | (_, d_dp2) = mul (d_di2, (p2, 2))
i3 = mul (i2, q)  | (d_di2, d_dq) = mul (d_di3, (q, i2))
i4 = mul (3, r)   | (_, d_dr) = mul (d_di4, (r, 3))
v = add (i3, i4)  | (d_di3, d_di4) = dup(d_dv)
```

If we form a new program consisting of the sequence of assignments on
the left followed by the sequence of assignments on the right *in
reverse order* then we have a program that calculates the reverse
derivative!  The "inputs" of this program are `x`, `y` and `d_dv`.
The "outputs" are `v`, `d_dx` and `d_dy`.

(Note that, similar to how in forward mode we omitted derivatives of
constants because they are zero, in reverse mode we omit the
calculation of derivatives *with respect to* constants because they
have no effect on the rest of the program.)

### The use of the duplication property

The explicit duplication property is important because in the code
generated by the reverse mode transformation, usages in the original
program become definitions in the reverse mode program;
correspondingly, definitions in the original program become usages in
the reverse mode program.  Therefore it is important that there is
exactly one of each: a variable cannot have two definitions!  For
example, consider the original source assignment `(x1, x2) = dup x`.
`x` is "used" in this line, and `x1` and `x2` are "defined".  It gives
rise to the assignment `d_dx = add (d_x1, d_dx2)` in the reverse mode
program.  `d_dx` is "defined" in this line and `d_dx1` and `d_dx2` are
"used".

### Properties of the reverse derivative

Again we see an important property of the automatic derivative: it
runs in time proportional to the run time of the original program
(assuming that the derivative of every primitive runs in time
proportional to the run time of the primitive itself).

If we look carefully we can also see another property of the
reverse derivative: it might use space proportional to the run time of
the original program!  Notice that the value of `x1` needs to be kept
around throughout the lifetime of the program so that we can calculate
`d_dx`.  Once we've calculated a value we can't just use it and throw
it away, like we could in forward mode.  (There's a technique called
"checkpointing" to address this which prefers to rerun computations
rather than store their results, decreasing space usage but increasing
run time.  [Siskind and Pearlmutter](https://arxiv.org/abs/1708.06799)
have a useful introduction to checkpointing.)

## What this does not cover

This is a description of how to differentiate what are called
"straight-line" programs, that is it does not cover recursion or
loops, or conditionals.  Arrays are not explicitly treated here
either, although they fit naturally into this framework.  Dealing
properly with those concepts requires extending the presentation given
here into something which would require a significantly longer
article.

An in-depth analysis of the sense in which the resulting programs are
the "derivative" of the input program will have to wait for another
article.

## Conclusion

Source-to-source forward and reverse mode automatic differentiation
can be expressed as follows

1. Apply simple transformations to get your program into a form where
   it can be differentiated line-by-line.

2. Apply the differentiation rule for each line separately.

   The forward mode differentiation rules are quite close to what you
   might already be familiar with.  The reverse mode rules are
   probably not, which might go some way to explaining why the reverse
   mode derivative has a reputation for being very mysterious.

## Questions?

If you have any questions then please [contact
me](http://web.jaguarpaw.co.uk/~tom/contact).

## Acknowledgements

Thanks to Mark Saroufim and Pashmina Cameron for helpful feedback.

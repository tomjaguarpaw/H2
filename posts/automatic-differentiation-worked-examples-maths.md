# Automatic differentiation: the maths

The article contains the maths needed to justify the transformations
in [Automatic differentiation: source-to-source worked
examples](../automatic-differentiation-worked-examples/).

## Derivative in terms of map of differentials

For full generality our differentiation will need to be able to handle
functions that take multiple values as arguments and return multiple
values.  Let's consider an example of multivariate differential
calculus, that is, partial differentiation, using the following
equation

\\[
(x, y) = (r \\cos \\theta, r \\sin \\theta)
\\]

(which happens to be the change of 2d coordinates from polar to
rectangular but that's not important here).  If you are familiar with
multivariable calculus then you will be able to write down the partial
derivatives

$$
\frac{\partial x}{\partial r} = \cos \theta; \,\,
\frac{\partial x}{\partial \theta} = -r \sin \theta; \,\,
\frac{\partial y}{\partial r} = \sin \theta; \,\,
\frac{\partial y}{\partial \theta} = r \cos \theta
$$

This collection of facts could well be considered to be "the"
derivative of the above equation but for our purposes we need to go
one step further and combine the facts above with the multivariate
chain rule of differential calculus.  Let's see what that means.

Suppose that \\(r\\) and \\(\\theta\\) themselves were functions of a
single real variable \\(t\\).  Then by the chain rule we would have

$$
\left(\frac{dx}{dt}, \frac{dy}{dt} \right)
= \left(\frac{\partial x}{\partial r} \frac{dr}{dt}
+ \frac{\partial x}{\partial \theta} \frac{d\theta}{dt}, \,
\frac{\partial y}{\partial r} \frac{dr}{dt}
+ \frac{\partial y}{\partial \theta} \frac{d\theta}{dt}\right)
$$

Because \\(t\\) is arbitrary I'm going to take a liberty with notation to
keep \\(t\\) implicit and write

$$
\left(dx, dy \right)
= \left(\frac{\partial x}{\partial r} dr
+ \frac{\partial x}{\partial \theta} d\theta, \,
\frac{\partial y}{\partial r} dr
+ \frac{\partial y}{\partial \theta} d\theta\right)
$$

This liberty serves to make the notation a bit neater.  If you prefer
to mentally insert \\(\\frac{}{dt}\\) in several places to get back to
the original form then please do so.

Substituting in the partial derivatives we calculated above we get

\\[
\\left(dx, dy \\right)
= \\left(\\cos \\theta \\, dr
- r \\sin \\theta \\, d\\theta, \\,
\\sin \\theta \\, dr
+ r \\cos \\theta \\, d\\theta\\right)
\\]

This is the derivative of our original equation (in terms of the
partial derivatives) *combined* with the chain rule.  Every assignment
in our program has a derivative that can be written in this form.

It might be interesting to note that one can express the same thing in
matrix form as

$$
\left(
\begin{array}{c}
dx \\
dy
\end{array}
\right)
=
\left(
\begin{array}{cc}
\frac{\partial x}{\partial r} & \frac{\partial x}{\partial \theta} \\
\frac{\partial y}{\partial r} & \frac{\partial y}{\partial \theta}
\end{array}
\right)
\left(
\begin{array}{c}
dr \\
d\theta
\end{array}
\right)
=
\left(
\begin{array}{cc}
\cos \theta & -r \sin \theta \\
\sin \theta &  r \cos \theta
\end{array}
\right)
\left(
\begin{array}{c}
dr \\
d\theta
\end{array}
\right)
$$

This is sometimes called the "Jacobian-vector product".

Combining the derivative with the chain rule gave us new variables
(\\(dx\\), \\(dy\\), \\(dr\\) and \\(d\\theta\\)) that will appear in
our forward mode derivative code.  I'm going to say, without further
explanation, that what we have written down is a formula for the
"differentials" (\\(dx\\) and \\(dy\\)) of the output variables in
terms of the "differentials" (\\(dr\\) and \\(d\\theta\\)) of the
input variables, and this is the form of derivative that we will be
using in the forward mode AD algorithm.  I won't go more into this
here because it really deserves an article (or more) all of its own,
but I hope examples will make it easy enough to become comfortable
with the pattern.

## Derivative in terms of map of gradients

The reverse mode derivative transformation will proceed by
differentiating individual lines of our source program, just like the
forward mode transformation.  Instead of generating the map of
"differentials" we are going to generate the map of "gradients" by
applying the chain rule in the opposite direction.

Let's recall our example multi-variable function from above.  Its
definition was

\\[
(x, y) = (r \\cos \\theta, r \\sin \\theta)
\\]

and its partial derivatives were

$$
\frac{\partial x}{\partial r} = \cos \theta; \,\,
\frac{\partial x}{\partial \theta} = -r \sin \theta; \,\,
\frac{\partial y}{\partial r} = \sin \theta; \,\,
\frac{\partial y}{\partial \theta} = r \cos \theta
$$

Suppose there were an \\(\\alpha\\) which was a function of \\(x\\)
and \\(y\\).  Then by the chain rule we would have

$$
\left(\frac{\partial \alpha}{\partial r},
\frac{\partial \alpha}{\partial \theta} \right)
= \left(\frac{\partial \alpha}{\partial x} \frac{\partial x}{\partial r}
+ \frac{\partial \alpha}{\partial y} \frac{\partial y}{\partial r},
\frac{\partial \alpha}{\partial x} \frac{\partial x}{\partial \theta}
+ \frac{\partial \alpha}{\partial y} \frac{\partial y}{\partial \theta}\right)
$$

Again I will slightly abuse notation to write

$$
\left(\frac{\partial}{\partial r},
\frac{\partial}{\partial \theta} \right)
= \left(\frac{\partial x}{\partial r} \frac{\partial}{\partial x}
+ \frac{\partial y}{\partial r} \frac{\partial}{\partial y},
\frac{\partial x}{\partial \theta} \frac{\partial}{\partial x}
+ \frac{\partial y}{\partial \theta} \frac{\partial}{\partial y}\right)
$$

And in terms of the partial derivatives given above this is

$$
\left(\frac{\partial}{\partial r},
\frac{\partial}{\partial \theta} \right)
= \left(\cos \theta \frac{\partial}{\partial x}
+ \sin\theta \frac{\partial}{\partial y},
-r \sin \theta \frac{\partial}{\partial x}
+ r \cos \theta \frac{\partial}{\partial y}\right)
$$

Now it is definitely interesting to express the same thing in matrix
form because we can see that it corresponds to matrix multiplication
in the opposite direction from the forward mode example

$$
\left(
\begin{array}{cc}
\frac{\partial}{\partial r} & \frac{\partial}{\partial \theta}
\end{array}
\right)
=
\left(
\begin{array}{cc}
\frac{\partial}{\partial x} & \frac{\partial}{\partial y}
\end{array}
\right)
\left(
\begin{array}{cc}
\frac{\partial x}{\partial r} & \frac{\partial x}{\partial \theta} \\
\frac{\partial y}{\partial r} & \frac{\partial y}{\partial \theta}
\end{array}
\right)
=
\left(
\begin{array}{cc}
\frac{\partial}{\partial x} & \frac{\partial}{\partial y}
\end{array}
\right)
\left(
\begin{array}{cc}
\cos \theta & -r \sin \theta \\
\sin \theta &  r \cos \theta
\end{array}
\right)
$$

This is sometimes called the "vector-Jacobian product".

I'm going to say, without further explanation, that what we have
written down is a formula for the "gradient" of the *input* variables
(\\(\\frac{\\partial}{\\partial r}\\) and
\\(\\frac{\\partial}{\\partial \\theta}\\)) in terms of the "gradient"
of the *output* variables (\\(\\frac{\\partial}{\\partial x}\\) and
\\(\\frac{\\partial}{\\partial y}\\)).  In other words this form of
the derivative works in the opposite direction to the original
function.

## Correspondence between the two forms

Recall that in the first section we wrote down a formula for the
"differentials" (\\(dx\\) and \\(dy\\)) of the output variables in
terms of the "differentials" (\\(dr\\) and \\(d\\theta\\)) of the
input variables

$$
\left(
\begin{array}{c}
dx \\
dy
\end{array}
\right)
=
\left(
\begin{array}{cc}
\frac{\partial x}{\partial r} & \frac{\partial x}{\partial \theta} \\
\frac{\partial y}{\partial r} & \frac{\partial y}{\partial \theta}
\end{array}
\right)
\left(
\begin{array}{c}
dr \\
d\theta
\end{array}
\right)
$$

and in the second section we wrote down a formula for the "gradient"
of the *input* variables (\\(\\frac{\\partial}{\\partial r}\\) and
\\(\\frac{\\partial}{\\partial \\theta}\\)) in terms of the "gradient"
of the *output* variables (\\(\\frac{\\partial}{\\partial x}\\) and
\\(\\frac{\\partial}{\\partial y}\\)).

$$
\left(
\begin{array}{cc}
\frac{\partial}{\partial r} & \frac{\partial}{\partial \theta}
\end{array}
\right)
=
\left(
\begin{array}{cc}
\frac{\partial}{\partial x} & \frac{\partial}{\partial y}
\end{array}
\right)
\left(
\begin{array}{cc}
\frac{\partial x}{\partial r} & \frac{\partial x}{\partial \theta} \\
\frac{\partial y}{\partial r} & \frac{\partial y}{\partial \theta}
\end{array}
\right)
$$

These two forms combine to give the following identity relating the
gradients and differentials of the input to the gradients and
differentials of the output.

$$
\left(
\begin{array}{cc}
\frac{\partial}{\partial x} & \frac{\partial}{\partial y}
\end{array}
\right)
\left(
\begin{array}{c}
dx \\
dy
\end{array}
\right)
=
\left(
\begin{array}{cc}
\frac{\partial}{\partial x} & \frac{\partial}{\partial y}
\end{array}
\right)
\left(
\begin{array}{cc}
\frac{\partial x}{\partial r} & \frac{\partial x}{\partial \theta} \\
\frac{\partial y}{\partial r} & \frac{\partial y}{\partial \theta}
\end{array}
\right)
\left(
\begin{array}{c}
dr \\
d\theta
\end{array}
\right)
=
\left(
\begin{array}{cc}
\frac{\partial}{\partial r} & \frac{\partial}{\partial \theta}
\end{array}
\right)
\left(
\begin{array}{c}
dr \\
d\theta
\end{array}
\right)
$$

## Determining the differentiation rules for primitives

Let's see how we use the above mathematics to derive differentiation
rules for primitive functions.

### Addition

Firstly consider addition, specifically \\(y = x_1 + x_2\\).
Taking partial derivatives we obtain

$$
\frac{\partial y}{\partial x_1} = 1 \, ; \, \frac{\partial y}{\partial x_2} = 1
$$

Now if \\(x_1\\) and \\(x_2\\) were functions of a single variable
\\(t\\) then the chain rule would give us that

$$
\frac{dy}{dt} = \frac{\partial y}{\partial x_1} \frac{dx_1}{dt}
              + \frac{\partial y}{\partial x_2} \frac{dx_2}{dt}
$$

or, in a slightly abbreviated form

$$
dy = \frac{\partial y}{\partial x_1} dx_1
   + \frac{\partial y}{\partial x_2} dx_2
$$

Plugging in the values above we obtain \\(dy = dx_1 + dx_2\\).  This
is the form we will use for the forward derivative of addition.  Now
apply the chain rule to \\(\\alpha\\), a hypothetical function of
\\(y\\)

$$
\left(\frac{\partial\alpha}{\partial x_1},
      \frac{\partial\alpha}{\partial x_2}\right)
  = \left(\frac{d\alpha}{dy} \frac{\partial y}{\partial x_1},
          \frac{d\alpha}{dy} \frac{\partial y}{\partial x_2}\right)
$$

which in abbreviated form is

$$
\left(\frac{\partial}{\partial x_1},
      \frac{\partial}{\partial x_2}\right)
  = \left(\frac{d}{dy} \frac{\partial y}{\partial x_1},
          \frac{d}{dy} \frac{\partial y}{\partial x_2}\right)
$$

Plugging in the values above we obtain

$$
\left(\frac{\partial}{\partial x_1},
      \frac{\partial}{\partial x_2}\right)
= \left(\frac{d}{dy}, \frac{d}{dy}\right)
$$

This is a perfectly valid way of expressing the derivative of
addition!  It will probably seem very strange -- we've never seen
anything like this in our multivariate calculus courses -- but it is a
form of the derivative of addition.  It is the form of the derivative
of addition that we will use in our reverse mode code.

### Multiplication

Now let's consider multiplication, specifically \\(y = x_1 x_2\\).
Taking partial derivatives we obtain

$$
\frac{\partial y}{\partial x_1} = x_2 \, ; \,
\frac{\partial y}{\partial x_2} = x_1
$$

Following the same recipe as above we deduce that the form of
derivative that we will use for multiplication in forward mode is
\\(dy = x_2 \\, dx_1 + x_1 \\, dx_2\\) and the form we will use in
reverse mode is

$$
\left(\frac{\partial}{\partial x_1},
\frac{\partial}{\partial x_2} \right)
= \left(x_2 \, \frac{d}{dy},
x_1 \, \frac{d}{dy} \right)
$$

Again this does not look like the derivative of multiplication, but it
is!

### Division

Consider \\(y = x_1 / x_2\\).  The partial derivatives are

$$
\frac{\partial y}{\partial x_1} = \frac{1}{x_2} \, ; \,
\frac{\partial y}{\partial x_2} = -\frac{x_1}{x_2^2}
$$

Therefore the forward mode derivative is \\(dy = dx_1 / x_2 - (x_1 /
x_2^2) \\, dx_2\\) and the reverse mode derivative is

$$
\left(\frac{\partial}{\partial x_1},
\frac{\partial}{\partial x_2} \right)
= \left(\frac{1}{x_2} \, \frac{d}{dy},
-\frac{x_1}{x_2^2} \, \frac{d}{dy} \right)
$$

### Duplication

Duplication corresponds to the mathematical function \\((y_1, y_2) =
(x, x)\\).  The partial derivatives are

$$
\frac{dy_1}{dx} = 1 \, ; \, \frac{dy_2}{dx} = 1
$$

Therefore the forward mode derivative is \\((dy_1, dy_2) = (dx, dx)\\)
and the reverse mode derivative is

$$
\frac{\partial}{\partial x}
= \frac{\partial}{\partial y_1}
+ \frac{\partial}{\partial y_2}
$$

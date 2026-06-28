# Stuff I wish that Python had

* The ability to introduce a new scope when binding something

  I really want to be able to make local bindings when assigning to a
  variable without polluting the outer scope.  For example, I want to
  be able to do the equivalent of

      let x = y ^ 2
          where y = 2 * p + 6


  (where `p` is from an outer scope).  It turns out there's a PEP for
  this: [PEP 3150](https://www.python.org/dev/peps/pep-3150/).

* I want to be able to define anonymous functions that are more than a
  single expression.

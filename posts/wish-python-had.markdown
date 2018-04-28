# Stuff I wish that Python had

* The ability to introduce a new scope when binding something

  I really want to be able to make local bindings when assigning to a
  variable without polluting the outer scope.  For example, I want to
  be able to do the equivalent of

      let x = y ^ 2
          where y = 2 * p + 6


  (where `p` is from an outer scope).

* I want to be able to define anonymous functions that are more than a
  single expression.

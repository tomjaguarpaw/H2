# Applicatives without Applicative

It seems that for an `Applicative` `f`, `f a` should be equivalent to
`(a -> IO ()) -> f (IO ())`.  Is this correct?  If so it might be
useful in the design of some APIs.  If not, is there a type we can
substitute for `IO ()`?  Maybe `IO Dynamic`?

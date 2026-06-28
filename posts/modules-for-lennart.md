# Faking modules in Haskell via implicit parameters

Many years ago [Lennart Augustsson wrote about the difficulty of
faking an ML-module-like structure in
Haskell](http://augustss.blogspot.se/2008/12/somewhat-failed-adventure-in-haskell.html).
The aim is to define an abstract "signature" of types and operations,
allow users to write derived functionality based on this signature,
and then later instantiate the derived functionality by providing an
instantiation for the signature.

Let's take an example where the module signature declares a
string-like and a double-like type, and declares operations to
concatenate the strings and to "show" the doubles by converting them
to strings.  Lennart's overall idea is to write something like the
following.

    -- Our module "signature" represented by a package of operations
    -- parametrised on types
    data DOps xstring xdouble = DOps {
       (+++) :: xstring -> xstring -> xstring,
       xshow :: xdouble -> xstring
       }
    
    -- Class for passing the package of operations to our derived
    -- functions
    class (IsString xstring, Num xdouble) => Ops xstring xdouble where
       ops :: DOps xstring xdouble
    
    -- Derived datatypes parametrised on the types introduced by our
    -- module
    data Person xstring xdouble = Person {
       firstName :: xstring,
       lastName  :: xstring,
       height    :: xdouble
       }

    -- Derived functions use the `Ops` class to receive the package of
    -- operations
    display :: (Ops xstring xdouble) =>
               Person xstring xdouble -> xstring
    display p = let DOps{..} = ops
                in  firstName p +++ " " +++ lastName p
                                +++ " " +++ xshow (height p + 1)

    -- Instantiate the signature by providing concrete types and
    -- operations
    instance Ops String Double where
       ops = DOps (++) show

This is actually a partially decent solution.  The drawback that
Lennart points out is that having to mention the type parameters
everywhere is an impediment to modularity.  In particular, if we need
to add another datatype to our module every single call site needs to
be updated to reflect that change.

Furthermore, because `Ops` is only parametrised on `xstring` and
`xdouble` we cannot provide two different packages of functions with
the same concrete types.

The obvious trick to try to get around these two problems is
identifying the typeclass using a single parameter, like the following


    class (IsString (XString t), Num (XDouble t)) => Ops t where
       type XString t :: *
       type XDouble t :: *
       (+++) :: XString t -> XString t -> XString t
       xshow :: XDouble t -> XString t
    
    data Person t = Person {
       firstName :: XString t,
       lastName  :: XString t,
       height    :: XDouble t
       }

    data Basic

    instance Ops Basic where
       type XString Basic = String
       type XDouble Basic = Double
       (+++) = (++)
       xshow = show

    display :: Ops t => Person t -> XString t
    display p = let DOps{..} = ops
                ...

However, this has instance resolution problems.  There is not
necessarily a unique instance to use to unpack the operations.  There
may be many `t0`s that satisfy `XString t0 = XString t` (as well as
the other necessary type equalities inside the function) and in those
cases `Ops t0` would do just as well as the `Ops t` instance.

## Implicit parameters to the rescue

I don't know if since Lennart wrote his post there have been type
system improvements that would allow him to achieve his aim with small
adjustments to his approach based on typeclasses and type families.
[In a subsequent
post](http://augustss.blogspot.se/2008/12/abstraction-continues-i-got-several.html)
Lennart mentions that Wehr and Chakravarty introduced a concept of
"abstract associated types" that might help with the problem.  That
functionality doesn't exist in any Haskell compiler though, as far as
I know.

Still, there is a related approach to this problem based on another
Haskell extension.  Perhaps surprisingly, replacing the typeclass
constraint with an implicit parameter seems to do the trick!  Here's
the proof.

    {-# LANGUAGE TypeFamilies, ImplicitParams, OverloadedStrings #-}
    {-# LANGUAGE NoMonomorphismRestriction #-}
    
    import Data.String (IsString)
    
    type family XString t
    type family XDouble t
    
    data DOps t = DOps {
      (+++!) :: XString t -> XString t -> XString t,
      xshowOp  :: XDouble t -> XString t
    }
    
    (+++) :: (?ops :: DOps t) => XString t -> XString t -> XString t
    (+++) = (+++!) ?ops
    
    xshow :: (?ops :: DOps t) => XDouble t -> XString t
    xshow = xshowOp ?ops
    
    data Person t = Person {
      firstName :: XString t,
      lastName  :: XString t,
      height    :: XDouble t
    }
    
    -- We don't need to provide a type signature but the inferred one
    -- is a bit messy
    --
    -- displayI
    --  :: (Num (XDouble t1), IsString (XString t1), ?ops::DOps t,
    --      XString t ~ XString t1, XDouble t ~ XDouble t1) =>
    --     Person t1 -> XString t1
    display p = firstName p +++ " " +++ lastName p
                            +++ " " +++ xshow (height p + 1)
    
    -- We can provide a nicer type signature
    display' :: (Num (XDouble t), IsString (XString t), ?ops::DOps t)
             => Person t -> XString t
    display' = display
    
    -- We need NoMonomorphismRestriction to be able to do this without
    -- a type sig
    display'' = display'

The key observation is that there is no ambiguity problem because we
are passing (implicitly) a single specific package of operations to
`display` which is used by each "module" function call in its body.

## Conclusion

This approach uses the maligned implicit parameters.  Despite that is
this a decent approach to getting some of the benefits of modules in
Haskell?

This is certainly not anything close to a module system, but does it
shed some light on the issue?

## A simple disambiguation

I've also discovered that a simple disambiguation can make the
single-parameter version work.

    {-# LANGUAGE RecordWildCards #-}
    {-# LANGUAGE MultiParamTypeClasses #-}
    {-# LANGUAGE TypeSynonymInstances #-}
    {-# LANGUAGE FlexibleContexts #-}
    {-# LANGUAGE FlexibleInstances #-}
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE DataKinds #-}
    {-# LANGUAGE TypeFamilies #-}
    
    import Data.String
    
    type family XString k
    type family XDouble k
    
    -- Our module "signature" represented by a package of operations
    -- parametrised on types
    data DOps k = DOps {
       (+++) :: XString k -> XString k -> XString k,
       xshow :: XDouble k -> XString k
       }
    
    -- Class for passing the package of operations to our derived
    -- functions
    class (IsString (XString k), Num (XDouble k)) => Ops k where
       ops :: DOps k
    
    -- Derived datatypes parametrised on the types introduced by our
    -- module
    data Person k = Person {
       firstName :: XString k,
       lastName  :: XString k,
       height    :: XDouble k
       }
    
    -- Use typeOf to disambiguate the typeclass instance
    typeOf :: f k -> g k -> f k
    typeOf f _ = f
    
    -- Derived functions use the `Ops` class to receive the package of
    -- operations
    --
    -- The type is inferred, and is breathtakingly simple
    --
    --     display :: Ops k => Person k -> XString k
    --
    display p = let DOps{..} = ops `typeOf` p
                in  firstName p +++ " " +++ lastName p
                                +++ " " +++ xshow (height p + 1)
    
    -- Instantiate the signature by providing concrete types and
    -- operations
    data Basic
    
    instance Ops Basic where
       ops = DOps (++) show
    
    type instance XString Basic = String

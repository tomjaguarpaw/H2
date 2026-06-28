# Why does Haskell not have an `exists` keyword?

In Haskell we can use `Rank2Types` or `RankNTypes` to pass arguments
which contain universally quantified parameters, for example

    {-# LANGUAGE Rank2Types #-}
    {-# LANGUAGE ExistentialQuantification #-}
    
    showBoth :: (Show b, Show c)
             => (forall a. Show a => a -> String) -> (b, c)
             -> (String, String)
    showBoth myShow (b, c) = (myShow b, myShow c)
    
    example1 :: (String, String)
    example1 = showBoth show (1, "foo")
    
We can also make datatypes that contain existential variables, that
means we know they have *some* type, but we don't know which.

    data Showable = forall a. Show a => Showable a
    
    showShowable :: Showable -> String
    showShowable (Showable a) = show a
    
    showAll :: [Showable] -> [String]
    showAll = map showShowable
    
    example2 :: [String]
    example2 = showAll [Showable "bar", Showable 5, Showable True]

However, what we'd really like to do is this:

    showAll :: [exists a. Show a => a] -> [String]
    showAll = map show

Why can't we do this?  Well, the reason that we can manipulate
`forall` is that it desugars directly into something in the
polymorphic lambda calculus (System F).  The type

       (Show b, Show c)
    => (forall a. Show a => a -> String) -> (b, c)
    -> (String, String)

actually means

       (Show b, Show c)
    => (/\a -> Show a => a -> String) -> (b, c)
    -> (String, String)

(modulo desugaring of the typeclass contexts).  Here `/\` is a
higher-level lambda, and its use means that the caller of the argument
(i.e. the body of `showBoth`) gets to choose to instantiate the `a` to
possibly many different types and use that function at all of them.
On the other hand, there is nothing for `exists` to directly desugar
to.  We would like the type

    [exists a. Show a => a] -> [String]

could mean something like

    [{Show a => a, a}] -> [String]

where the `{...}` notation indicates a dependent sum.  However, there
is no such construction available in Haskell's Core intermediate
language.

I certainly recall seeing these dependent products in some
presentations of polymorphic lambda calculus so perhaps there is no
impediment to them in principle.

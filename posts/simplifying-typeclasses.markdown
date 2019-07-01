# Simplifying typeclasses

The current implementation of typeclasses in GHC/Haskell is quite complex, as they have a variety of
different features. A small portion of these features are fundamental to typeclasses and cannot be
removed, but many of these features already exist in other forms in Haskell, and those other forms
should be used instead.

These features include:

* Defining a typeclass

* Defining superclasses of typeclasses

* Defining instances of typeclasses

* Defining structures of typeclasses

* Defining a typeclass instance by defining only a subset of its structure

* Deriving typeclasses based on the structure of a type

* Deriving typeclasses based on an existing instance that type has

* Deriving typeclasses on a newtype by coercing the instances of the underlying type

* Deriving typeclasses based on a newtype that is a member of that typeclass

* Convert between instances by direct manipulation of their structure

I am going to argue that only the first 3 of the above features are necessary, and that the rest can
be done using existing Haskell features. We can thus reduce the featureset of typeclasses
substantially to just the first 3 plus the needed hooks to interact with the rest of Haskell. This
should be simpler and easier and actually more expressive, as regular Haskell is more expressive
than the featureset built around typeclasses.

### Defining a typeclass

Typeclasses are fundamentally different than regular data types, so we need a separate syntax of
some sort to define them.

```
class Eq a
```

### Defining superclasses of typeclasses

Haskell doesn't really have any notion of subtyping outside of typeclasses, so this is also pretty
fundamental.

```
class Eq a => Ord a
```

### Defining instances of typeclasses

Classes can be thought of as open and extensible functions from the type level to the value level,
thus instance declerations are extending this function with specific clauses. Again pretty unique
and fundamental.

```
instance Eq Int
```

### Defining structures of typeclasses

Haskell already has a concept of "structure" in the form of algabreic data types, so instead of
defining structures of typeclasses separately, we should just specify in the form of a regular
Haskell type what the underlying structure of a typeclass is.

Old:

```
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a
```

```
class Monoid a = monoid :: Monoid_ a
data Monoid_ a = Monoid
    { mempty :: a
    , mappend :: a -> a -> a
    }
```

### Defining a typeclass instance by defining only a subset of its structure

Haskell already allows you to do this for regular data structures through smart constructors. So
instead of defining a bunch of mutually recursive definitions and requiring an extra `MINIMAL`
pragma you can simply expose as many smart constructors as you would like.

Old:

```
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
    {-# MINIMAL (==) | (/=) #-}

instance Eq Int where
    x == y = ...

instance Eq Bool where
    x /= y = ...
```

New:

```
class Eq a = (==) :: a -> a -> Bool

(/=) :: Eq a => a -> a -> Bool
x /= y = not (x == y)

invertRel :: (a -> a -> Bool) -> a -> a -> Bool
invertRel f x y = not (f x y)

instance Eq Int = ...

instance Eq Bool = invertRel ...
```

### Deriving typeclasses based on the structure of a type

I am referring to GHC knowing how to derive Eq, Ord, Show, Generic etc.

This cannot be solved by this current proposal alone, but with extensible rows/records/variants etc.
all new nominal types will be newtypes over some underyling structure, and thus the newtype section
of this proposal will apply.

### Deriving typeclasses based on an existing instance that type has

This is currently done by defining a default implementation in the class itself, so that
DeriveAnyClass can use it. This is particularly useful for typeclasses that can be built on the
underlying structure of a type via Generic. This approach has the obvious and rather annoying
limitation of only allowing one class to be used as a way to build another, whereas many classes
can be built in multiple different ways (Traversable -> Functor) (Applicative -> Functor).

Instead of this approach you can simply define functions that convert directly from one typeclass
structure to another typeclass structure. This allows you to define as many different conversions
between as many pairs of typeclasses you want. You can also make classes as desired for various
common patterns of converting between typeclasses, to avoid having to import as many names.

Old:

```
instance Monad Foo where
    (>>=) = ...

instance Applicative Foo where
    pure = ...
    (<*>) = ap

instance Functor Foo where
    fmap = liftA

data Bar = ...
    deriving (Generic, FromJSON, ToJSON)
```

New:

```
instance Monad Foo = Monad
    { mBind = ...
    , mPure = ...
    }
instance Applicative Foo = monadToApplicative monad
instance Functor Foo = monadToFunctor monad

data Bar = ... deriving Generic
instance FromJSON Bar = genericToFromJSON generic
instance ToJSON Bar = genericToToJSON generic
```

### Deriving typeclasses on a newtype by coercing the instances of the underlying type

This is currently done via GeneralizedNewtypeDeriving, which coerces every member of the underlying
instance into a valid member of the new instance. This implementation should be simpler with this
new approach as the entire structure can be coerced at once. It also makes the newtype deriving less
important, as you can just use coerce explicitly.

Old:

```
newtype Foo = Foo Int
    deriving (Bar, Baz)
```

New:

```
newtype Foo = Foo Int
instance Bar Foo = coerce (bar @Int)
instance Baz Foo = coerce (baz @Int)
```

### Deriving typeclasses based on a newtype that is a member of that typeclass

This is currently achieved with DerivingVia, which generalizes GeneralizedNewtypeDeriving to allow
for the use of types other than the underyling type. However an extra extension and new syntax would
not be needed with this new approach, as you can simply adjust the explicit coerces above:

Old:

```
newtype Foo = Foo Int
    deriving Monoid via (Sum Int)
```

New:

```
newtype Foo = Foo Int
instance Monoid Foo = coerce (monoid @(Sum Int))
```

### Convert between instances by direct manipulation of their structure

This isn't really possible in current Haskell, you essentially just have to manipulate each
individual class member one by one. See things like `fmapDefault`. One might argue DerivingVia
achieves this by allowing you to define a newtype for each conversion you need, but this is not
expressive enough to count as true direct manipulation. For example if you can define many different
conversions from `Foo` to `Bar` each parameterized by an Int or a String, suddenly DerivingVia
cannot help you. I also don't like the ergonomics of defining a brand new top level type for every
possible conversion when a function is much more direct and lightweight.

## Proposal

The above can more or less be acheieved in current Haskell, simply by choosing to only ever give
typeclasses a single member. So my proposal to the community is to do exactly that.

With that said there are some ergonomic improvements that can be made, such as the `=` based syntax
I use above:

Old:

```
class Eq a where (==) :: a -> a -> Bool

instance Eq Int where (==) = ...
```

New:

```
class Eq a = (==) :: a -> a -> Bool

instance Eq Int = ...
```

Extensible rows/records/variants etc. and associated dot syntax sugar would also help with the
ergonomics of the above, as you wouldn't have to come up with a separate name for the top level
typeclass member and the field of the underlying data type. See: `mBind` and `mPure` above.

The long term proposal would be for everyone to move away from the old approach and stop using
various typeclass extensions and features, until eventually they can be deprecated and then
removed. I can understand if some prefer the old syntax for some of the above, but note that using
only single member typeclasses would not prevent use of the old syntax, so if the community prefers
keeping various typeclass features they don't necessarily have to be removed.

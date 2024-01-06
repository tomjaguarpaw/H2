{-# OPTIONS_GHC -Wall #-}

import Control.Monad hiding (foldM)
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import qualified Data.Foldable
import Data.Functor.Const
import Data.Monoid
import Prelude hiding (foldl, foldr)

foldl_ :: (b -> a -> b) -> b -> [a] -> b
foldl_ _ z [] = z
foldl_ f z (a : as) = foldl_ f (f z a) as

foldr_ :: (a -> b -> b) -> b -> [a] -> b
foldr_ _ z [] = z
foldr_ f z (a : as) = f a (foldr_ f z as)

foldlFromForState ::
  (forall a b. [b] -> (b -> State a ()) -> State a ()) ->
  forall a b.
  (a -> b -> a) ->
  a ->
  [b] ->
  a
foldlFromForState for_ f z bs = flip evalState z $ do
  for_ bs $ \b -> do
    a <- get
    put (f a b)
  get

forStateFromFoldl ::
  (forall a b. (a -> b -> a) -> a -> [b] -> a) ->
  forall a b.
  [b] ->
  (b -> State a ()) ->
  State a ()
forStateFromFoldl foldl bs f = do
  z <- get
  put (foldl g z bs)
  where
    g a b = execState (f b) a

type EndoApplicative a = Const (Endo a)

mkEndoApplicative :: (a -> a) -> EndoApplicative a ()
mkEndoApplicative = Const . Endo

runEndoApplicative :: a -> EndoApplicative a () -> a
runEndoApplicative a (Const (Endo f)) = f a

foldrFromFor ::
  (forall b f. (Applicative f) => [b] -> (b -> f ()) -> f ()) ->
  forall a b.
  (b -> a -> a) ->
  a ->
  [b] ->
  a
foldrFromFor for_ f z bs =
  runEndoApplicative z $ for_ bs $ \b -> mkEndoApplicative (f b)

forFromFoldr ::
  (forall a b. (b -> a -> a) -> a -> [b] -> a) ->
  forall b f.
  (Applicative f) =>
  [b] ->
  (b -> f ()) ->
  f ()
forFromFoldr foldr bs f =
  foldr (\b rest -> f b *> rest) (pure ()) bs

foldMFromForStateT ::
  ( forall a b m.
    (Monad m) =>
    [b] ->
    (b -> StateT a m ()) ->
    StateT a m ()
  ) ->
  forall a b m.
  (Monad m) =>
  (a -> b -> m a) ->
  a ->
  [b] ->
  m a
foldMFromForStateT for_ f z bs = flip evalStateT z $ do
  for_ bs $ \b -> do
    a <- get
    put =<< lift (f a b)
  get

forStateTFromFoldlM ::
  ( forall a b m.
    (Monad m) =>
    (a -> b -> m a) ->
    a ->
    [b] ->
    m a
  ) ->
  forall a b m.
  (Monad m) =>
  [b] ->
  (b -> StateT a m ()) ->
  StateT a m ()
forStateTFromFoldlM foldM bs f = do
  z <- get
  put =<< lift (foldM g z bs)
  where
    g a b = execStateT (f b) a

foldlFromFoldr ::
  (forall a b. (b -> a -> a) -> a -> [b] -> a) ->
  forall a b.
  (a -> b -> a) ->
  a ->
  [b] ->
  a
foldlFromFoldr foldr = foldlFromForState (forFromFoldr foldr)

foldlFromFoldr2 ::
  (forall a b. (b -> a -> a) -> a -> [b] -> a) ->
  forall a b.
  (a -> b -> a) ->
  a ->
  [b] ->
  a
foldlFromFoldr2 foldr f' z bs' =
  flip evalState z $ do
    for_ bs' $ \b -> do
      a <- get
      put (f' a b)
    get
  where
    for_ bs f = foldr (\b rest -> f b *> rest) (pure ()) bs

foldlFromFoldr3 ::
  (forall a b. (b -> a -> a) -> a -> [b] -> a) ->
  forall a b.
  (a -> b -> a) ->
  a ->
  [b] ->
  a
foldlFromFoldr3 foldr f' z bs' =
  flip evalState z $ do
    for_ bs' g
    get
  where
    for_ bs f = foldr (\b rest -> f b *> rest) (pure ()) bs
    g b = do
      a <- get
      put (f' a b)

foldlFromFoldr4 ::
  (forall a b. (b -> a -> a) -> a -> [b] -> a) ->
  forall a b.
  (a -> b -> a) ->
  a ->
  [b] ->
  a
foldlFromFoldr4 foldr f z bs =
  flip evalState z $ do
    foldr (\b rest -> g b *> rest) (pure ()) bs
    get
  where
    g b = do
      a <- get
      put (f a b)

foldlFromFoldr45 ::
  (forall a b. (b -> a -> a) -> a -> [b] -> a) ->
  forall a b.
  (a -> b -> a) ->
  a ->
  [b] ->
  a
foldlFromFoldr45 foldr f z bs =
  flip evalState z $ do
    foldr (\b rest -> g b *> rest) (pure ()) bs
    get
  where
    g b = modify (flip f b)

foldlFromFoldr5 ::
  (forall a b. (b -> a -> a) -> a -> [b] -> a) ->
  forall a b.
  (a -> b -> a) ->
  a ->
  [b] ->
  a
foldlFromFoldr5 foldr f z bs =
  flip evalState z $ do
    foldr (\b rest -> modify (flip f b) *> rest) (pure ()) bs
    get

foldlFromFoldr6 ::
  (forall a b. (b -> a -> a) -> a -> [b] -> a) ->
  forall a b.
  (a -> b -> a) ->
  a ->
  [b] ->
  a
foldlFromFoldr6 foldr f z bs =
  flip execState z $ do
    foldr (\b rest -> modify (flip f b) *> rest) (pure ()) bs

foldlFromFoldr7 ::
  (forall a b. (b -> a -> a) -> a -> [b] -> a) ->
  forall a b.
  (a -> b -> a) ->
  a ->
  [b] ->
  a
foldlFromFoldr7 foldr f z bs =
  flip execState z $ do
    modify $
      foldr
        (\b rest -> execState $ modify (flip f b) *> modify rest)
        (execState (pure ()))
        bs

foldlFromFoldr75 ::
  (forall a b. (b -> a -> a) -> a -> [b] -> a) ->
  forall a b.
  (a -> b -> a) ->
  a ->
  [b] ->
  a
foldlFromFoldr75 foldr f z bs =
  flip execState z $ do
    modify $
      foldr
        (\b rest -> execState $ modify (rest . flip f b))
        (execState (pure ()))
        bs

foldlFromFoldr76 ::
  (forall a b. (b -> a -> a) -> a -> [b] -> a) ->
  forall a b.
  (a -> b -> a) ->
  a ->
  [b] ->
  a
foldlFromFoldr76 foldr f z bs =
  flip execState z $ do
    modify $
      foldr
        (\b rest -> rest . flip f b)
        (execState (pure ()))
        bs

foldlFromFoldr8 ::
  (forall a b. (b -> a -> a) -> a -> [b] -> a) ->
  forall a b.
  (a -> b -> a) ->
  a ->
  [b] ->
  a
foldlFromFoldr8 foldr f z bs =
  execState
    ( modify $
        foldr
          (\b rest -> rest . flip f b)
          (execState (pure ()))
          bs
    )
    z

foldlFromFoldr9 ::
  (forall a b. (b -> a -> a) -> a -> [b] -> a) ->
  forall a b.
  (a -> b -> a) ->
  a ->
  [b] ->
  a
foldlFromFoldr9 foldr f z bs =
  foldr
    (\b rest -> rest . flip f b)
    (execState (pure ()))
    bs
    z

foldlFromFoldr10 ::
  (forall a b. (b -> a -> a) -> a -> [b] -> a) ->
  forall a b.
  (a -> b -> a) ->
  a ->
  [b] ->
  a
foldlFromFoldr10 foldr f z bs =
  foldr (\b rest a -> rest (f a b)) id bs z

foo ::
  (b1 -> b2) ->
  (b2 -> b1) ->
  (t -> b2 -> b2) ->
  b2 ->
  ([t] -> b2, [t] -> b2)
foo h g f z =
  (foldr_ (\a -> f a) z, h . foldr_ (\a -> g . f a . h) (g z))

(!?) :: [a] -> Int -> Maybe a
xs !? n =
  fromEither $ do
    flip evalStateT 0 $ do
      Data.Foldable.for_ xs $ \a -> do
        i <- get
        when (n == i) (lift (Left (Just a)))
        put (i + 1)
    Left Nothing

fromEither :: Either a a -> a
fromEither = either id id

printEvens :: [Int] -> IO ()
printEvens =
  Data.Foldable.foldr (\i rest -> when (even i) (print i) *> rest) (pure ())

printEvensFor :: [Int] -> IO ()
printEvensFor is = Data.Foldable.for_ is $ \i -> when (even i) (print i)

printEvensFoldl :: [Int] -> IO ()
printEvensFoldl =
  Data.Foldable.foldl
    (\rest i -> when (even i) (print i) *> rest)
    (pure ())
    . reverse

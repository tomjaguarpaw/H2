# An `IO` transformer

[It has been
claimed](http://stackoverflow.com/questions/13056663/why-is-there-no-io-transformer-in-haskell)
that there is no `IO` transformer in Haskell.  This seems overly
pessimistic.  The obvious approach is to use

    newtype IOT m a = IOT (m (IO a))

However, we get stuck when we try to define

    join :: Monad m => m (IO (m (IO a))) -> m (IO a)

The typical approach would be to try to commute the `IO (m ...)` to `m
(IO ...)` but there's just no way to do that.  We can't even do it for
some simple choices of `m` like `Maybe` let alone uniformly for all
`Monad`s `m`.

But there's a another way.  If we can turn all the `m`s into `IO` then
we can just join them all together into one `IO` action.  This
suggests an `IO` transformer like

    newtype IOT m a = IOT ((forall a. m a -> IO a) -> IO a)

In fact, it's just a use of the `Reader` transformer.

    newtype Morph f g = Morph { runMorph :: forall a. (f a -> g a) }
    newtype IOT m a = IOT { runIOT :: ReaderT (Morph m IO) IO a }

Using `ReaderT` here is just a convenience that means we automatically
get a `Monad` for free.  It doesn't play a role of any substance.

    instance Monad (IOT m) where
      return = IOT . return
      m >>= f = IOT (runIOT m >>= (runIOT . f))

The `MonadTrans` instance is

    instance MonadTrans IOT where
      lift act = IOT $ do
        f <- ask
        lift (runMorph f act)

That is, we just convert all the `m` actions into `IO` actions.  (Note
that the latter `lift` is in `ReaderT`).  Does it satisfy [the
`MonadTrans`
laws](http://hackage.haskell.org/package/transformers-0.3.0.0/docs/Control-Monad-Trans-Class.html)?
Well (eliding `runMorph` and `IOT` wrapping/unwrapping for notational
convenience),

    lift (return x) = do
        f <- ask
        lift (f (return x))

which is

    lift (return x) = do
        f <- ask
        return x

in the case where `f (return x)` = `return x`, i.e. `lift . return =
return`.  Additionally

    lift (m >>= g) = do
        f <- ask
        lift (f (m >>= g))

which is

    lift (m >>= g) = do
        f <- ask
        x <- lift (f m)
        f' <- ask
        lift (f' (g x))

i.e.

    lift m >>= \x -> lift (g x)

that is

    lift m >>= (lift . g)


in the case where `f (m >>= g) = f m >>= (f . g)`.  So `lift (m >>= g)
= lift m >>= (lift . g)`.

Thus we see that `IOT` is a monad transformer *if the reader
environment is a monad morphism* `f` satisfying

* `f (return x) = return x`
* `f (m >>= g) = f m >>= (f . g)`

# How to use `IOT`

You can run an `IOT m` action by providing a way of interpreting an
`m` action in `IO`.  (Remember the interpretation has to be a monad
morphism for the monad laws to hold).

    runIOT' :: (forall a. m a -> IO a) -> IOT m a -> IO a
    runIOT' f m = runReaderT (runIOT m) (Morph f)

It seems that there are several uses of this transformer.

## `Maybe`

For `m = Maybe` we can throw an error for `Nothing`.

    handleMaybe :: Maybe a -> IO a
    handleMaybe Nothing = error "Nothing"
    handleMaybe (Just a) = return a
    
    maybeExample :: IO ()
    maybeExample = runIOT' handleMaybe $ do
      liftIO (putStrLn "Hello")
      lift Nothing
      liftIO (putStrLn "Goodbye")

    > maybeExample 
    Hello
    *** Exception: Nothing

## `Either`

For `m = Either e` we can throw an error which depends on the
`Left` value.

    handleEither :: Show e => Either e a -> IO a
    handleEither (Left e) = error (show e)
    handleEither (Right a) = return a
    
    eitherExample :: IO ()
    eitherExample = runIOT' handleEither $ do
      liftIO (putStrLn "Hello")
      lift (Left 1234)
      liftIO (putStrLn "Goodbye")

    > eitherExample 
    Hello
    *** Exception: 1234

## `Writer`

For `m = Writer [w]` we can print a log message.

    handleWriter :: Show w => Writer [w] a -> IO a
    handleWriter m = mapM_ print ws >> return a
      where (a, ws) = runWriter m
    
    writerExample :: IO ()
    writerExample = runIOT' handleWriter $ do
      liftIO (putStrLn "Hello")
      lift (tell ["log1", "log2"])
      liftIO (putStrLn "Goodbye")
      lift (tell ["log3"])

    > writerExample 
    Hello
    "log1"
    "log2"
    Goodbye
    "log3"

## `State s`

For `m = State s` we can use an `IORef` to hold the state.

    stateExample :: IO ()
    stateExample = do
      ref <- newIORef 0
      let handleState :: State Integer a -> IO a
          handleState m = do
            s <- readIORef ref
            let (a, s') = runState m s
            writeIORef ref s'
            return a
    
      runIOT' handleState $ do
        x <- lift get
        liftIO (print x)
        lift (modify (+1))
        y <- lift get
        liftIO (print y)

    > stateExample 
    0
    1

# Is this really an `IO` transformer?

Firstly note that `IOT Maybe`, `IOT (Either e)` and `IOT (State s)`
seem to give strictly less power than `MaybeT IO`, `EitherT e IO` and
`StateT s IO` respectively.  `IOT (Writer [w])` does seem to be
different to `WriterT [w] IO` in that it preserves the ordering of
`IO` and `Writer` actions.

But is there some definition for "`M` transformer", for an arbitrary
monad `M`?  And what is the justification for saying that Haskell has
no `IO` transformer?  Even though it may not be especially useful,
this `IOT` seems like a reasonable candidate for one.

# The code

    {-# LANGUAGE Rank2Types #-}
    
    import Control.Monad.Trans.Reader
    import Control.Monad.Trans.Writer
    import Control.Monad.Trans.State
    import Control.Monad.Trans.Class
    import Data.IORef
    
    newtype Morph f g = Morph { runMorph :: forall a. (f a -> g a) }
    
    newtype IOT m a = IOT { runIOT :: ReaderT (Morph m IO) IO a }
    
    instance Monad (IOT m) where
      return = IOT . return
      m >>= f = IOT (runIOT m >>= (runIOT . f))
    
    instance MonadTrans IOT where
      lift act = IOT $ do
        f <- ask
        lift (runMorph f act)
    
    liftIO :: IO a -> IOT m a
    liftIO = IOT . lift
    
    handleMaybe :: Maybe a -> IO a
    handleMaybe Nothing = error "Nothing"
    handleMaybe (Just a) = return a
    
    runIOT' :: (forall a. m a -> IO a) -> IOT m a -> IO a
    runIOT' f m = runReaderT (runIOT m) (Morph f)
    
    maybeExample :: IO ()
    maybeExample = runIOT' handleMaybe $ do
      liftIO (putStrLn "Hello")
      lift Nothing
      liftIO (putStrLn "Goodbye")
    
    handleEither :: Show e => Either e a -> IO a
    handleEither (Left e) = error (show e)
    handleEither (Right a) = return a
    
    eitherExample :: IO ()
    eitherExample = runIOT' handleEither $ do
      liftIO (putStrLn "Hello")
      lift (Left 1234)
      liftIO (putStrLn "Goodbye")
    
    handleWriter :: Show w => Writer [w] a -> IO a
    handleWriter m = mapM_ print ws >> return a
      where (a, ws) = runWriter m
    
    writerExample :: IO ()
    writerExample = runIOT' handleWriter $ do
      liftIO (putStrLn "Hello")
      lift (tell ["log1", "log2"])
      liftIO (putStrLn "Goodbye")
      lift (tell ["log3"])
    
    stateExample :: IO ()
    stateExample = do
      ref <- newIORef 0
      let handleState :: State Integer a -> IO a
          handleState m = do
            s <- readIORef ref
            let (a, s') = runState m s
            writeIORef ref s'
            return a
    
      runIOT' handleState $ do
        x <- lift get
        liftIO (print x)
        lift (modify (+1))
        y <- lift get

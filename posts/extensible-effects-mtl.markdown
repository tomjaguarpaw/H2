# Further examples for extensible-effects

A continuation of [a previous page on
extensible-effects](../extensible-effects-interleaving/).

## extensible-effects

Oleg suggested to me the following example which shows how easily one
can abstract the client in the Extensible Effects framework:

    c5 = runTrace $ runReader (loop =<< runC (th client)) (10::Int)
     where loop (Y x k) = trace (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
           loop Done    = trace "Done"
    
           -- cl, client, ay are monomorphic bindings
           client = ay >> ay
           ay     = ask >>= \x -> yield (x::Int)
    
           -- There is no polymorphic recursion here
           th cl = do
             cl
             v <- ask
             (if v > (20::Int) then id else local (+(5::Int))) cl
             if v > (20::Int) then return () else local (+(10::Int)) (th cl)
    {-
    The expected result is:
    10
    11
    16
    16
    21
    21
    21
    21
    Done
    -}

## mtl

The same result can be achieved with the MTL, but the `CoT` type needs
a `MonadFunctor` instance so that we can use `hoist`.  I don't know if
`CoT` as implemented via `ContT` can be given such an instance.  As
such I implemened my own version like this

    data Step y m a = Done a | Yield y (CoT y m a)

    data CoT y m a = CoT (m (Step y m a))

and then we can reimplement `c5` straightforwardly

    c5 :: IO ()
    c5 = runReaderT (loop =<< runC (th client)) (10::Int)
     where loop (Yield x k) = (liftIO . print) (show (x::Int)) >> local (+(1::Int)) (runC k) >>= loop
           loop (Done _)    = (liftIO . print) "Done"
    
           -- Type signatures are not required, but I will
           -- say what they are anyway.
           
    --     client :: (MonadCo r m, MonadReader r m) => m ()
           client = ay >> ay
    --     ay :: (MonadCo r m, MonadReader r m) => m ()
           ay     = ask >>= yieldG
    
    --     th :: (Monad m, MFunctor t, Monad (t (ReaderT Int m)),
    --            MonadReader Int (t (ReaderT Int m)))
    --           => t (ReaderT Int m) () -> t (ReaderT Int m) ()
           th cl = do
             cl
             v <- ask
             (if v > (20::Int) then id else hoist (localLocal' (+(5::Int)))) cl
             if v > (20::Int) then return () else hoist (localLocal' (+(10::Int))) (th cl)

The reimplementation is remarkably similar to the original with just
some `hoist`s added.  Running it gives exactly the result desired

    GHCi> c5
    "10"
    "11"
    "16"
    "16"
    "21"
    "21"
    "21"
    "21"
    "Done"

## Further questions

The concept of `MFunctor` and its method `hoist` seem to play a
crucial role here.  In the case of a free monad I would guess that the
`MFunctor` instance comes for free, i.e. free monads always support
`hoist`.  This seems to be a crucial missing element from the MTL to
allow effects to work with each other in a generic manner.  I need to
think about this more ...


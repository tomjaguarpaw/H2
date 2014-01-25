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

However, with very little cost (`Rank2Types` and the associated
awkwardness of type inference) one can implement the same
functionality with `mtl`'s typeclasses using exactly the same
code structure:

    -- Even more dynamic example in mtl
    c5 :: IO ()
    c5 = runReaderT (loop =<< runC (th client)) (10::Int)
     where loop (Y x k) = (liftIO . print) (show (x::Int)) >> local (+(1::Int)) (k ()) >>= loop
           loop Done    = (liftIO . print) "Done"
    
           -- cl, client, ay have to be monad polymorphic in the mtl case
           client :: (MonadCo r m, MonadReader r m) => m ()
           client = ay >> ay
           ay :: (MonadCo r m, MonadReader r m) => m ()
           ay     = ask >>= yieldG
    
           -- th takes a rank-2 polymorphic argument in the mtl case
           th :: (forall m. (MonadCo r m, MonadReader Int m) => m ())
                 -> ((MonadCo r m, MonadReader Int m) => m ())
           th cl = do
             cl
             v <- ask
             -- vv Rank2Types make things somewhat more awkward here
             if v > (20::Int) then cl else localLocal (+(5::Int)) cl

Running it gives exactly the result desired

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

## Similarity

The open union interface of `extensible-effects` is actually somewhat
similar to the typeclass interface of `mtl`.  Observe that in the
`extensible-effects` case, `ay` has the type

    (Member (Yield Int) r, Member (Reader Int) r)) => Eff r ()

whereas in the `mtl` case the type is

    (forall m. (MonadCo r m, MonadReader r m) => m ())

`Member` constraints in `extensible-effects` correspond quite
naturally to `Monad...` constraints in `mtl`, and a value with type

    (MonadX m, MonadY m, MonadZ m) => m a

can have its effects X, Y and Z interpreted in any order, simply
by choosing the order of `runX`, `runY` and `runZ` operations.



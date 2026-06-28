# `ListT` done wrong

The typical definition definition of `ListT`, for example [from
`transformers`](http://hackage.haskell.org/package/transformers-0.3.0.0/docs/Control-Monad-Trans-List.html#v:ListT),
is

    newtype ListT m a = ListT (m [a])

The definition of `join` for this type is

1. given a `m [m [a]]`
2. run the outer action to get a `[m [a]]`
3. run all the inner actions to get a `[[a]]`
4. `concat` (i.e. `join`) the list of lists to get a `[a]`

Pictorially, join turns this tree

         A
        /|\
       / | \
      B1 B2 ...
     /|\ |\
    p q..x y ...
    
into this tree

    A, B1, B2, ...
     / /   \ \   
    p q ... x y ...

That is, the actions `A`, `B1`, `B2`, ... are all run in order and
their results are concatenated.

That type checks, but it doesn't satisfy the monad law `join . fmap
join = join . join`.  To see this, suppose we have the following
structure of three nested levels of `ListT`:

      A
     / \ 
    B   D
    |   |
    C   E
    |   |
    ()  ()

`join` results in

      A, B, D
       /  \
      C    E
      |    |
      ()   ()

and a further `join` results in

    A, B, D, C, E
       /   \
      ()   ()

On the other hand, if we `fmap join` first we get

          A
         / \
        /   \
    B, C    D, E
      |      |
      ()     ()

and a further `join` gives

       A, B, C, D, E
           /     \
          ()     ()

So `join . fmap join` gives us a different order of `m` actions than
`join . join`, hence the requirement that "`m` should be a commutative
monad".

## `ListT` done right

* [http://www.haskell.org/haskellwiki/ListT_done_right](http://www.haskell.org/haskellwiki/ListT_done_right)
* [http://www.haskellforall.com/2013/03/pipes-32-listt-codensity-arrowchoice.html](http://www.haskellforall.com/2013/03/pipes-32-listt-codensity-arrowchoice.html)

## Code

Here's some code you can run to check the claims in this article.

    import Control.Monad (join)    
    import Control.Monad.Trans (lift)
    import Control.Monad.Trans.List (ListT(ListT), runListT)
    import Control.Monad.Trans.Writer (tell, execWriter, Writer)
        
    foo :: ListT (Writer String)    
           (ListT (Writer String)    
            (ListT (Writer String) ()))    
    foo = do    
        lift (tell "A")    
        ListT (return [bar, baz])    
        
    bar :: ListT (Writer String) (ListT (Writer String) ())    
    bar = do lift (tell "B")    
             return (lift (tell "C"))    
        
    baz :: ListT (Writer String) (ListT (Writer String) ())    
    baz = do lift (tell "D")    
             return (lift (tell "E"))    
        
    go1 = (execWriter . runListT . join . fmap join) foo    
    -- "ABCDE"
              
    go2 = (execWriter . runListT . join . join) foo
    -- "ABDCE"

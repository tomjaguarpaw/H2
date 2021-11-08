# Why do we like lazy lists?

* `take 10 . sort` has optimized asymptotic time

    <https://www.reddit.com/r/haskell/comments/6h84vg/when_do_we_really_need_lazy_lists/dixfnn0/>

* `take k` of a lazily produced list

    <https://www.reddit.com/r/haskell/comments/6h84vg/when_do_we_really_need_lazy_lists/dixgvj9/>

* Ed Kmett's memo table

* General argument that they are better than fingertrees in some situations

    <https://www.reddit.com/r/haskell/comments/6h84vg/when_do_we_really_need_lazy_lists/dj0zblk/>

* `zip [1..] (...)`

    <https://www.reddit.com/r/haskell/comments/6h84vg/when_do_we_really_need_lazy_lists/diw9hmx/>


*  `zip (cycle [True, False])` and `take n (cycle xs)`

    <https://www.reddit.com/r/haskell/comments/6h84vg/when_do_we_really_need_lazy_lists/diwnhua/>

* The alternative is just to make lists spine strict, but that makes guarded recursion fly out the window, the following for example will run much slower, and take asymptotically more (O(n) vs O(1)) stack space:

    ```haskell
    fmap f (x : xs) = f x : fmap f xs
    fmap _ [] = []
    ```

    traverse and basically any function that builds a list will similarly suffer, e.g toList, many, some etc.

    <https://www.reddit.com/r/haskell/comments/6h84vg/when_do_we_really_need_lazy_lists/dizksf1/>

* `mapM XXX . fliter YYY . [AAA...BBB]` is meant to be running at constant space, and strict list will simply blow the heap.

    <https://www.reddit.com/r/haskell/comments/6h84vg/when_do_we_really_need_lazy_lists/dixkihy/>


* Something to do with Alternative

    <https://www.reddit.com/r/haskell/comments/6h84vg/when_do_we_really_need_lazy_lists/diwhzwf/>


Lennart's points

* Cyclic data structures

* Reuse:

```haskell
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p
```

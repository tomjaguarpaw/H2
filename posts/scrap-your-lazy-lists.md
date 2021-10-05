# Scrap your lazy lists

Instead of lazy lists you probably want a streaming abstraction, such as

* `streaming`
* `pipes`
* `conduit`

or a strict linear data structure, such as

* `Seq`
* `Array`
* `Vector`

Using lazy lists leads to bugs such as

* <https://www.reddit.com/r/haskell/comments/8b7jkd/awful_memory_consumption_when_using_lazy/>

[Haskell Reddit on when we really need lazy
lists](https://www.reddit.com/r/haskell/comments/6h84vg/when_do_we_really_need_lazy_lists/dj13bn0/)

The benefits are

1. [Laziness is more "reusable" or
   "composable"](https://www.reddit.com/r/haskell/comments/6h84vg/when_do_we_really_need_lazy_lists/dixkt23/)

   See [Lennart's write
   up](http://augustss.blogspot.com.br/2011/05/more-points-for-lazy-evaluation-in.html>)

2. Ed Kmett's [memo table](https://stackoverflow.com/a/3209189)
   (N.B. this has linear lookup time though -- the memo table based on
   trees is much better)

3. `take n . sort` (although this is possible with streams too, I
   think)

4. As a unifying type for `Alternative`'s `many`

   See
   <https://www.reddit.com/r/haskell/comments/6h84vg/when_do_we_really_need_lazy_lists/diwhzwf/>

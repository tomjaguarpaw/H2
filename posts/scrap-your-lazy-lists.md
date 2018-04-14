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

* https://www.reddit.com/r/haskell/comments/8b7jkd/awful_memory_consumption_when_using_lazy/

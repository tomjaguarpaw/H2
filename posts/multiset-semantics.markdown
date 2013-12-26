# Multiset semantics

Here's an example of why relational databases need multiset (or
multirelation) semantics, rather than set (or relation) semantics.

Suppose I have two tables:

* Apples with columns `owner :: String` and `quantity :: Int`
* Oranges with columns `owner :: String` and `quantity :: Int`

If I want to work out how many fruits each owner has I can do

  select owner, sum(quantity) from (apples union all oranges) group by owner

I can't do the same with `union` in the place of `union all` because if an
owner has the same number of apples as oranges then the `union` will
coalesce those two rows into one!

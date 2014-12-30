# Multiset semantics

Here's an example of why relational databases need multiset (or
multirelation) semantics, rather than set (or relation) semantics.

Suppose I have two tables:

* Apples with columns `owner :: String` and `quantity :: Int`
* Oranges with columns `owner :: String` and `quantity :: Int`

If I want to work out how many fruits each owner has I can do

    SELECT owner, SUM(quantity)
    FROM (apples UNION ALL oranges)
    GROUP BY owner

I can't do the same with `UNION` in the place of `UNION ALL` because
if an owner has the same number of apples as oranges then the `UNION`
will coalesce those two rows into one!  In set semantics we would have
to artificially add arbitrary identifiers to the `apples` and
`oranges` tables so the `UNION` can distinguish their rows, and
discard the identifier after aggregation.

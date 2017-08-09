# Programming as if the Correct Data Structure (and Performance) Mattered

## Abstract

What if focusing on the correct data structure, while still understanding
your algorithm, could improve maintainability and collaterally speed up
execution by a factor of over 15x compared to over-engineered C# code?

## Introduction

Once upon a time, I saw this problem come up in a [misguided
polemic](https://drive.google.com/file/d/0B59Tysg-nEQZUkdRT2lfUVM3cVk):

> You are given an input array whose each element represents the height of a
> line towers.  The width of every tower is 1.  It starts raining.  How much
> water is collected between the towers?

## Cutting a long story short

You can write a neat and correct solution in Haskell.

```haskell
rainfall :: [Int] -> Int
rainfall xs = sum (zipWith (-) mins xs)
  where
    mins = zipWith min maxl maxr
    maxl = scanl1 max xs
    maxr = scanr1 max xs
```

The trouble is, it's quite slow (7.2s &ndash; including setting up the
world, since you didn't exclude that from your benchmark).  From here
you have two options.  Your first option is to write a long rant about
how slow this code is and share it on the internet.  Your second
option is to use the right data structure and get a 114x speed up
(61ms) for almost zero effort.

```haskell
rainfall' :: Vector Int -> Int
rainfall' xs = V.sum (V.zipWith3 (\l r x -> (min l r) - x) maxl maxr xs)
  where
    maxl = V.scanl1' max xs
    maxr = V.scanr1' max xs
```

Notably, it's also 15x faster than some over-engineered C# code.  The
drawback of the fast Haskell code is that it's not so easy to change if
your boss decides to shift the requirements under you because he
believes there are real towers filling with water and insists that he
wants to be able to drill holes in the walls.

## Conclusion

Use the correct data structure.  If you're not sure, hold back on the
sarcasm and ask [Porges](https://github.com/Porges) to help you out.  He
seems to be a smart cookie.

## Acknowledgements

* [Some nice code](https://gist.github.com/Porges/9ca15a9ec01bf055edcd88394496dbe3),
  Porges

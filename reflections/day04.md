Today was a nice set theory question, and for that we need to parse some sets of jobs:

```haskell
parseElfPairJobs :: String -> (ElfJobs, ElfJobs)
parseElfPairJobs s =
    let [a,b,c,d] <- map read . splitOneOf ",-" $ s
        l = IS.fromList . Ix.range $ (a,b)
        r = IS.fromList . Ix.range $ (c,d)
     in (l,r)
```

We can get a set for each of the ranges of section ids in the elves' task lists.

[Data.Ix has a really helpful function `range`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Ix.html#v:range) for this sort of thing.

Then for part 1 we need to see if either is a subset of the other:

```haskell
day04a = length . filter (\(l,r) -> l `isSubsetOf` r || r `isSubsetOf` l) . map parseElfPairJobs . lines
```

Yay! One star! and for part two we just need to know if there is an intersection of the two:

```haskell
day04b = length . filter (\(l,r) -> not . null . intersection l r) . map parseElfPairJobs . lines
```

with that, the ship is spick & span.

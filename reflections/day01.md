My prewritten parser almost worked... just needed to install the split library...

But in any case parsing was relatively straight forward.
We simply need to split into the elf groups and the split the individual lines and read the numbers.

```haskell
parse :: String -> [[Int]]
parse = map (map read) . map lines . splitOn "\n\n"
```

To find the top one for part a I originally went for the following `foldMap` approach using the useful [`Max` Semigroup](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Semigroup.html#t:Max)

```haskell
solve :: [[Int]] -> Int
solve = getMax . foldMap (Max . sum)
```

Which, yay!, works but when we get to part 2 we can make more generic and reuse the part 2 implementation.

```haskell
solve :: [[Int]] -> Int
solve = sum . take 3 . reverse . sort . map sum
```

This solution does away with `Max` in favour of just sorting the summed list.

We can then pass in how many elves are contributing snacks:

```haskell
solve :: Int -> [[Int]] -> Int
solve n = sum . take n . reverse . sort . map sum
```

and tada! We've started our long journey[^1] into the jungle!

[^1]: _journey_ would be a great codename for a project

Either the challenges are harder to fool AI, or it's because the 1st is on a Sunday
this year, or maybe just that I'm one-year slower than before but the parsing took me longer than it should.

Nevertheless, the idea is straight-forward:
 1. Split on lines
 1. Split on whitespace to form pairs
 1. Unzip the list to get each groups list

```haskell
parse = fmap unzip
      . traverse (listTup <=< traverse readMaybe . splitOn "  ")
      . lines
```

of course, we do this all safely using the `Maybe` type to convert the strings to numbers and list into pairs.

For using the elves first checkign method of diffs we need to sort the two lists
and perform a pairwise diff, taking the absolute value of that diff.

```haskell
solve a b = zipWith score (sort a) (sort b)
    where
        score x y = abs $ x - y
```

and for their second checking method we can utilise our frequency map to turn the
second list into a frequency lookup and use that as a multiplier for each locationId.

```haskell
solve a b = map go a
    where
        b' = fromListWith (+) . map (,1) $ b
        go x = x * findWithDefault 0 x b'
```

and yet, the Chief Historian is nowhere to be found...

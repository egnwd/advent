Day 3
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day03.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *3* / *[4][day04]*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day01]: https://github.com/egnwd/advent/blob/2022/reflections-out/day01.md
[day02]: https://github.com/egnwd/advent/blob/2022/reflections-out/day02.md
[day04]: https://github.com/egnwd/advent/blob/2022/reflections-out/day04.md

*[Prompt][d03p]* / *[Code][d03g]*

[d03p]: https://adventofcode.com/2022/day/3
[d03g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day03.hs

Interesting, today was a day where I used a different parser for part 1 and 2.

For part 1 we can split it into lines, chop each in half and turn them into sets:

```haskell
splitHalf :: String -> (Set Char, Set Char)
splitHalf s = let h = length s `div` 2
                  (l, r) splitAt h s
               in (S.fromList l, S.fromList r)

parsea :: String -> [(Set Char, Set Char)]
parsea = map splitHalf . lines
```

Then solving is a simple map and score function:

```haskell
priority :: Char -> Int
priority c
    | isUpper c = 27 + (((subtract . ord) 'A') . ord) c
    | otherwise = 1  + (((subtract . ord) 'a') . ord) c

matchingItem :: (Set Char, Set Char) -> Char
matchingItem (r1, r2) = head . S.toList $ S.intersection r1 r2

solvea = sum . map (priority . matchingItem) . parsea
```

Yay! Rucksack's organized

Part 2 was similar, but we don't need to split in half, we need batches of 3.

```haskell
parseb :: String -> [[Set Char]]
parseb = chunksOf 3 . lines
```

solving is then just repeatedly intersecting the batches:

```haskell
findGroupBadge :: [Set Char] -> Char
findGroupBadge = head . S.toList . foldl1' S.intersection

solveb = sum . map (priority . findGroupBadge) . parseb
```

One thing with the above solution (which is very close to my initial attempt)
is that it doesn't do any error handling.

What if splitting a rucksack in half leaves an empty section?
What if there is a item we don't expect, `&` perhaps?
What if we have no matching item after all the intersections?

For my final implementation I added in some error handling which was good fun,
though for these sorts of problems it's not required as the inputs are well formed.

For starters, Items can only have priorities 1-52,
so I decided to use the `Finite` type which lets you restrict the number of inhabitants of the type.

```haskell
type Item = Finite 53
```

the 53 allows for the `0` inhabitant, but you'll see why I think that's fine in a second.

Now to create the `Item` type I can use

```haskell
priority :: Char -> Maybe Item
priority c
    | isUpper c = packFinite . fromIntegral $ 27 + (((subtract . ord) 'A') . ord) c
    | otherwise = packFinite . fromIntegral $ 1  + (((subtract . ord) 'a') . ord) c
```

You'll see the type change to reflect the fact this operation can fail.
I think 53 is fine as in both branches we add at least `1` so the `0` can never appear and we can sort of sweep it under the rug.

Next I dealt with the fact intersections can leave you with an empty set.

Rucksacks can now be defined as non empty sets of characters:

```haskell
type Rucksack = NESet Char
```

Defining the intersections now needs a little more work to deal with this error case:

```haskell
intersections :: Ord a => [NESet a] -> Maybe (Set a)
intersections = foldl1May S.intersection . fmap NES.toSet
```

First, turn the non empty sets into normal sets, run the intersection using a `foldl1May`...

`foldl1May` is a left fold that assumes there is at least 1 element in the list.
Normal `foldl1` will error if you don't:

```ghci
λ> foldl1 (+) [] :: Int
*** Exception: Prelude.foldl1: empty list
```

where as `foldl1May` returns `Nothing`:

```ghci
λ> foldl1May (+) [] :: Maybe Int
Nothing
```

This now returns a set, as all of the elements could have been removed in the intersections.

Finally, to get out a single element from the set could be risky if they were all removed.

For this we can use `Lens`es, which has a nice function `preview` that allows you to grab the first matching element if one exists of a folded or traversable.
Here we have sets and we don't want any special filters, so plain `preview folded` should do.


For how all this is strung together check out the code, linked from the top of this reflection.


*[Back to all reflections for 2022][reflections]*

## Day 3 Benchmarks

```
>> Day 03a
benchmarking...
time                 255.3 μs   (254.4 μs .. 256.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 254.9 μs   (254.5 μs .. 255.4 μs)
std dev              1.572 μs   (1.232 μs .. 2.181 μs)

* parsing and formatting times excluded

>> Day 03b
benchmarking...
time                 292.2 μs   (291.6 μs .. 292.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 291.9 μs   (291.4 μs .. 292.6 μs)
std dev              2.047 μs   (1.555 μs .. 2.879 μs)

* parsing and formatting times excluded
```

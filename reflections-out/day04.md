Day 4
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day04.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *4* / *[5][day05]* / *[6][day06]*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day01]: https://github.com/egnwd/advent/blob/2022/reflections-out/day01.md
[day02]: https://github.com/egnwd/advent/blob/2022/reflections-out/day02.md
[day03]: https://github.com/egnwd/advent/blob/2022/reflections-out/day03.md
[day05]: https://github.com/egnwd/advent/blob/2022/reflections-out/day05.md
[day06]: https://github.com/egnwd/advent/blob/2022/reflections-out/day06.md

*[Prompt][d04p]* / *[Code][d04g]*

[d04p]: https://adventofcode.com/2022/day/4
[d04g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day04.hs

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


*[Back to all reflections for 2022][reflections]*

## Day 4 Benchmarks

```
>> Day 04a
benchmarking...
time                 25.35 μs   (25.25 μs .. 25.46 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 25.33 μs   (25.25 μs .. 25.42 μs)
std dev              307.2 ns   (236.3 ns .. 423.7 ns)

* parsing and formatting times excluded

>> Day 04b
benchmarking...
time                 22.30 μs   (22.24 μs .. 22.35 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 22.29 μs   (22.25 μs .. 22.33 μs)
std dev              124.7 ns   (92.67 ns .. 166.1 ns)

* parsing and formatting times excluded
```

Day 1
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day01.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *1*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md


*[Prompt][d01p]* / *[Code][d01g]*

[d01p]: https://adventofcode.com/2022/day/1
[d01g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day01.hs

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


*[Back to all reflections for 2022][reflections]*

## Day 1 Benchmarks

```
>> Day 01a
benchmarking...
time                 25.66 μs   (25.62 μs .. 25.69 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 25.64 μs   (25.59 μs .. 25.68 μs)
std dev              143.8 ns   (109.5 ns .. 191.5 ns)

* parsing and formatting times excluded

>> Day 01b
benchmarking...
time                 26.61 μs   (26.40 μs .. 26.97 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 26.39 μs   (26.31 μs .. 26.54 μs)
std dev              346.7 ns   (183.7 ns .. 647.3 ns)

* parsing and formatting times excluded
```

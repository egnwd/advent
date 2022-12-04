Day 1
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day01.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *1* / *[2][day02]* / *[3][day03]* / *[4][day04]*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day02]: https://github.com/egnwd/advent/blob/2022/reflections-out/day02.md
[day03]: https://github.com/egnwd/advent/blob/2022/reflections-out/day03.md
[day04]: https://github.com/egnwd/advent/blob/2022/reflections-out/day04.md

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
time                 24.49 μs   (24.43 μs .. 24.58 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 24.53 μs   (24.50 μs .. 24.59 μs)
std dev              158.0 ns   (127.0 ns .. 192.4 ns)

* parsing and formatting times excluded

>> Day 01b
benchmarking...
time                 26.14 μs   (25.61 μs .. 27.30 μs)
                     0.993 R²   (0.982 R² .. 1.000 R²)
mean                 25.94 μs   (25.72 μs .. 27.12 μs)
std dev              1.304 μs   (239.6 ns .. 3.152 μs)
variance introduced by outliers: 58% (severely inflated)

* parsing and formatting times excluded
```

Day 1
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day01.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *1* / *[2][day02]*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day02]: https://github.com/egnwd/advent/blob/2024/reflections-out/day02.md

*[Prompt][d01p]* / *[Code][d01g]*

[d01p]: https://adventofcode.com/2024/day/1
[d01g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day01.hs

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


*[Back to all reflections for 2024][reflections]*

## Day 1 Benchmarks

```
>> Day 01a
benchmarking...
time                 320.5 μs   (313.4 μs .. 339.2 μs)
                     0.991 R²   (0.976 R² .. 1.000 R²)
mean                 316.1 μs   (313.4 μs .. 326.0 μs)
std dev              16.31 μs   (1.393 μs .. 34.59 μs)
variance introduced by outliers: 48% (moderately inflated)

* parsing and formatting times excluded

>> Day 01b
benchmarking...
time                 112.7 μs   (112.6 μs .. 112.7 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 112.8 μs   (112.7 μs .. 112.9 μs)
std dev              359.7 ns   (257.2 ns .. 538.1 ns)

* parsing and formatting times excluded
```

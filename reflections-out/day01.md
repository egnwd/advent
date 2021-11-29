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

[d01p]: https://adventofcode.com/2015/day/1
[d01g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day01.hs

Nice and simple to start off

Both parts involved read the string as a `[Int]` and we can then find the sum of the list.

```
getValue :: Char -> Int
getValue '(' = 1
getValue ')' = -1
```

```
λ> sum . map getValue $ "())"
-1
```

For part 2, we can fold over the list and keep track of the postition and the level,
when we hit the basement we can just return the position.


*[Back to all reflections for 2015][reflections]*

## Day 1 Benchmarks

```
>> Day 01a
benchmarking...
time                 22.63 μs   (21.96 μs .. 23.45 μs)
                     0.994 R²   (0.990 R² .. 0.999 R²)
mean                 22.33 μs   (22.01 μs .. 22.90 μs)
std dev              1.378 μs   (878.7 ns .. 2.095 μs)
variance introduced by outliers: 68% (severely inflated)

* parsing and formatting times excluded

>> Day 01b
benchmarking...
time                 39.39 μs   (39.19 μs .. 39.63 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 39.86 μs   (39.57 μs .. 40.72 μs)
std dev              1.630 μs   (772.6 ns .. 3.034 μs)
variance introduced by outliers: 46% (moderately inflated)

* parsing and formatting times excluded
```

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
time                 22.73 μs   (21.45 μs .. 24.49 μs)
                     0.982 R²   (0.969 R² .. 0.999 R²)
mean                 22.16 μs   (21.67 μs .. 22.88 μs)
std dev              1.909 μs   (1.242 μs .. 2.862 μs)
variance introduced by outliers: 81% (severely inflated)

* parsing and formatting times excluded

>> Day 01b
benchmarking...
time                 38.64 μs   (38.36 μs .. 38.97 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 38.47 μs   (38.36 μs .. 38.63 μs)
std dev              461.1 ns   (336.7 ns .. 706.0 ns)

* parsing and formatting times excluded
```

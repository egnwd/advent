Reflections
===========

<!--
This file generated by the build script at ./Build.hs from the files in
./reflections.  If you want to edit this, edit those instead!
-->

Table of Contents
-----------------

* [Day 1](#day-1)
* [Day 2](#day-2) *(no reflection yet)*

Day 1
------

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day01.md`.  If you want to edit this, edit
that file instead!
-->

*[Prompt][d01p]* / *[Code][d01g]* 

[d01p]: https://adventofcode.com/2015/day/1
[d01g]: https://github.com/egnwd/advent/blop/2015/src/AOC/Challenge/Day01.hs

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


### Day 1 Benchmarks

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



Day 2
------

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day02.md`.  If you want to edit this, edit
that file instead!
-->

*[Prompt][d02p]* / *[Code][d02g]* 

[d02p]: https://adventofcode.com/2015/day/2
[d02g]: https://github.com/egnwd/advent/blop/2015/src/AOC/Challenge/Day02.hs

*Reflection not yet written -- please check back later!*

### Day 2 Benchmarks

```
>> Day 02a
benchmarking...
time                 147.9 μs   (147.7 μs .. 148.3 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 148.8 μs   (148.4 μs .. 149.3 μs)
std dev              1.548 μs   (1.161 μs .. 1.949 μs)

* parsing and formatting times excluded

>> Day 02b
benchmarking...
time                 160.6 μs   (160.2 μs .. 160.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 160.4 μs   (160.2 μs .. 160.7 μs)
std dev              838.3 ns   (651.1 ns .. 1.248 μs)

* parsing and formatting times excluded
```

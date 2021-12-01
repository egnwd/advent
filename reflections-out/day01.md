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

[d01p]: https://adventofcode.com/2021/day/1
[d01g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day01.hs

Reading a list of ints always seems to be early on, we can do that easily with:

```haskell
parser = map read . lines
```

Then for part 1 we need to get the differences, which can be achieved by dropping the head of the
list then subtracting the two lists element-wise:

```haskell
differences x = zipWith subtract x (drop 1 x)
```

and then to get the answer we need only the increases we just filter for where we got a postitive result:

```haskell
part1 = length . filter (>0) . differences
```

Yay! 1 star!

For part 2 we can reuse the differences logic from part 1, we just need to construct a new input list using the windows.
The easiest way to do this given the window is only length 3 is use `zipWith` again, this time with addition.

```haskell
windowedInput x = zipWith (+) (zipWith (+) x (drop 1 x)) (drop 2 x)
```

Then to solve we just use part 1.

```haskell
part2 = part1 . windowedInput
```

Day 1 complete!


*[Back to all reflections for 2021][reflections]*

## Day 1 Benchmarks

```
>> Day 01a
benchmarking...
time                 44.27 μs   (43.35 μs .. 45.30 μs)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 43.47 μs   (42.88 μs .. 44.14 μs)
std dev              2.237 μs   (1.878 μs .. 2.593 μs)
variance introduced by outliers: 57% (severely inflated)

* parsing and formatting times excluded

>> Day 01b
benchmarking...
time                 86.93 μs   (85.04 μs .. 89.42 μs)
                     0.997 R²   (0.995 R² .. 0.998 R²)
mean                 88.99 μs   (87.57 μs .. 90.06 μs)
std dev              4.159 μs   (3.664 μs .. 4.798 μs)
variance introduced by outliers: 49% (moderately inflated)

* parsing and formatting times excluded
```

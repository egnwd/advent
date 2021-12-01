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
differences x = zipWith subtract x (tail x)
```

and then to get the answer we need only the increases we just filter for where we got a postitive result:

```haskell
part1 = length . filter (>0) . differences
```

Yay! 1 star!

For part 2 we can reuse the differences logic from part 1, we just need to construct a new input list using the windows.
The easiest way to do this given the window is only length 3 is use the version of `zipWith` that takes 3 lists, this time with addition.

```haskell
summedSlidingWindows x = zipWith3 (\a b c -> a + b + c) x (drop 1 x) (drop 2 x)
```

Then to solve we just use part 1.

```haskell
part2 = part1 . summedSlidingWindows
```

Day 1 complete!


*[Back to all reflections for 2021][reflections]*

## Day 1 Benchmarks

```
>> Day 01a
benchmarking...
time                 47.27 μs   (46.45 μs .. 48.43 μs)
                     0.994 R²   (0.990 R² .. 0.997 R²)
mean                 47.09 μs   (45.79 μs .. 48.64 μs)
std dev              4.412 μs   (3.534 μs .. 5.703 μs)
variance introduced by outliers: 82% (severely inflated)

* parsing and formatting times excluded

>> Day 01b
benchmarking...
time                 208.5 μs   (202.4 μs .. 214.6 μs)
                     0.994 R²   (0.990 R² .. 0.997 R²)
mean                 210.7 μs   (206.9 μs .. 216.4 μs)
std dev              15.33 μs   (10.99 μs .. 20.72 μs)
variance introduced by outliers: 67% (severely inflated)

* parsing and formatting times excluded
```

Day 7
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day07.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *7* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day01]: https://github.com/egnwd/advent/blob/2021/reflections-out/day01.md
[day02]: https://github.com/egnwd/advent/blob/2021/reflections-out/day02.md
[day03]: https://github.com/egnwd/advent/blob/2021/reflections-out/day03.md
[day04]: https://github.com/egnwd/advent/blob/2021/reflections-out/day04.md
[day05]: https://github.com/egnwd/advent/blob/2021/reflections-out/day05.md
[day06]: https://github.com/egnwd/advent/blob/2021/reflections-out/day06.md
[day08]: https://github.com/egnwd/advent/blob/2021/reflections-out/day08.md
[day09]: https://github.com/egnwd/advent/blob/2021/reflections-out/day09.md
[day10]: https://github.com/egnwd/advent/blob/2021/reflections-out/day10.md
[day11]: https://github.com/egnwd/advent/blob/2021/reflections-out/day11.md
[day12]: https://github.com/egnwd/advent/blob/2021/reflections-out/day12.md
[day13]: https://github.com/egnwd/advent/blob/2021/reflections-out/day13.md

*[Prompt][d07p]* / *[Code][d07g]*

[d07p]: https://adventofcode.com/2021/day/7
[d07g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day07.hs

I feel like there is a better solution for today that avoids checking all points, but for now here is the naïve version:

For part 1 we need a way of scoring a particular horizontal position.
The score here is the sum of the absolute differences from the crabs to the position in question.

```haskell
score :: [Int] -> Int -> Int
score crabs position = sum . map (abs . subtract position) $ crabs
```

The we need to check all the positions from the minimum crab to the maximum crab.

```haskell
solve [Int] -> Int
solve crabs = minimum . map (score crabs) $ [minimum cs .. maximum cs]
```

And there we go!

For part 2, then need to confess my original implementation of part 1 was:

```haskell
solve [Int] -> Int
solve crabs = minimum . map (score crabs) $ crabs
```

as I assumed we had to pick a crab for them to congregate on,
and one of my crabs was on the right spot.

For part 2 I then just needed to tweak the score function to be

```haskell
triangularNumber :: Int -> Int
triangularNumber x = x * (x + 1) `div` 2

score :: [Int] -> Int -> Int
score crabs position = sum . map (triangularNumber . abs . subtract position) $ crabs
```

_...and then you debug why you can't get the right answer and think you can no longer remember what a triangular number is for 20 minutes,
despite watching absurds amounts of [sudoku videos](https://www.youtube.com/c/CrackingTheCryptic)..._

...until you realise all the crabs could congregate on any of the spots.

_sigh._


*[Back to all reflections for 2021][reflections]*

## Day 7 Benchmarks

```
>> Day 07a
benchmarking...
time                 32.43 ms   (32.06 ms .. 32.90 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 31.91 ms   (31.36 ms .. 32.27 ms)
std dev              947.0 μs   (535.8 μs .. 1.565 ms)

* parsing and formatting times excluded

>> Day 07b
benchmarking...
time                 2.616 s    (NaN s .. 2.654 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.597 s    (2.591 s .. 2.607 s)
std dev              9.376 ms   (1.954 ms .. 12.41 ms)
variance introduced by outliers: 19% (moderately inflated)

* parsing and formatting times excluded
```

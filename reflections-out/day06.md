Day 6
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day06.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *6*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day01]: https://github.com/egnwd/advent/blob/2022/reflections-out/day01.md
[day02]: https://github.com/egnwd/advent/blob/2022/reflections-out/day02.md
[day03]: https://github.com/egnwd/advent/blob/2022/reflections-out/day03.md
[day04]: https://github.com/egnwd/advent/blob/2022/reflections-out/day04.md
[day05]: https://github.com/egnwd/advent/blob/2022/reflections-out/day05.md

*[Prompt][d06p]* / *[Code][d06g]*

[d06p]: https://adventofcode.com/2022/day/6
[d06g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day06.hs

My test cases hurt me today... arguably...

I got the right answer but failed some of the tests,
the rewrite cost me some time, but it is right now...

Anyway, today was a nice "one-liner" in Haskell:

After yesterday's parsing adventure today had... **none!**

so the solution for part 1 is:

```haskell
day06 =
    last                            -- Get the last index (the answer!)
    fst                             -- Get the indicies of the elements
    head                            -- Get first window satisfying the predicate
    . filter ((==4) . size . snd)   -- Filter to those windows of the correct size after deduplicating
    . map
        ( second fromList           -- Make a set out of the elements
        . unzip                     -- Make a tuple of the indicies and the elements
        . take 4)                   -- Force the windows to size 4 (the packet header size)
    . tails                         -- Make list of sliding windows
    . zip [1..]                     -- Add indicies
```

The only functions that might need some extra detail if you aren't familiar:
 - `zip`: take two lists and make a single list of pairwise elements
 - `unzip`: Take a list of tuples (`[(Int, Char)]` for example) and makea tuple of two lists `([Int], [Char])` i.e. the opposite of `zip`
 - `second`: Helper function from Bifunctor that applies a function `f` to the second element of a tuple.

Packet found!

Part 2 involves switching out (both 😴) `4` for `14`.

Ya-ftzzz, Ove-bpzts.


*[Back to all reflections for 2022][reflections]*

## Day 6 Benchmarks

```
>> Day 06a
benchmarking...
time                 311.3 μs   (302.5 μs .. 318.2 μs)
                     0.996 R²   (0.995 R² .. 0.998 R²)
mean                 304.8 μs   (300.9 μs .. 310.0 μs)
std dev              15.19 μs   (11.90 μs .. 17.28 μs)
variance introduced by outliers: 46% (moderately inflated)

* parsing and formatting times excluded

>> Day 06b
benchmarking...
time                 2.319 ms   (2.312 ms .. 2.330 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.322 ms   (2.315 ms .. 2.329 ms)
std dev              22.40 μs   (15.77 μs .. 28.95 μs)

* parsing and formatting times excluded
```

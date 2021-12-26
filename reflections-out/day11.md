Day 11
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day11.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *11* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day01]: https://github.com/egnwd/advent/blob/2021/reflections-out/day01.md
[day02]: https://github.com/egnwd/advent/blob/2021/reflections-out/day02.md
[day03]: https://github.com/egnwd/advent/blob/2021/reflections-out/day03.md
[day04]: https://github.com/egnwd/advent/blob/2021/reflections-out/day04.md
[day05]: https://github.com/egnwd/advent/blob/2021/reflections-out/day05.md
[day06]: https://github.com/egnwd/advent/blob/2021/reflections-out/day06.md
[day07]: https://github.com/egnwd/advent/blob/2021/reflections-out/day07.md
[day08]: https://github.com/egnwd/advent/blob/2021/reflections-out/day08.md
[day09]: https://github.com/egnwd/advent/blob/2021/reflections-out/day09.md
[day10]: https://github.com/egnwd/advent/blob/2021/reflections-out/day10.md
[day12]: https://github.com/egnwd/advent/blob/2021/reflections-out/day12.md
[day13]: https://github.com/egnwd/advent/blob/2021/reflections-out/day13.md
[day14]: https://github.com/egnwd/advent/blob/2021/reflections-out/day14.md
[day15]: https://github.com/egnwd/advent/blob/2021/reflections-out/day15.md
[day16]: https://github.com/egnwd/advent/blob/2021/reflections-out/day16.md

*[Prompt][d11p]* / *[Code][d11g]*

[d11p]: https://adventofcode.com/2021/day/11
[d11g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day11.hs

I again enjoyed the theme on this day, I liked the idea of the flashing octopuses.

Part 1 and 2 had a nice similar foundation and just had different stopping criteria.

I'll first explain my core logic, then cover the stopping criteria.

We need a function that can take a step for all the octopuses, then we iterate this step function.
Similar to how you might solve a conways game of life problem.

To define the step function we want something that takes the octopuses and gives us a new state of octopuses:

```haskell
type Octopuses = M.Map Point Int

step :: Octopuses -> Octopuses
step os = undefined
```

The algorithm we need steps the energy level of each octopus once,
then again for each octopus next to an octopus that flashed (note that if an
octopus is adjacent to 2 octopuses that flash, it flashes twice).

It does this until it settles (octopuses can flash only once per step)
and then the flashed octopuses reset their energy.

I broke this into two stages:

1. Finding the settled state (fixed point)
1. Resetting the grid

A fixed point takes a function from some state to another state of the same time,
and keeps applying this function (starting with some initial state) until it sees the same state twice,
and returns that state.

My fixed point function takes a frequency map of octopuses to give an energy boost, and how many times to do so, and the map of octopuses.

```haskell
next :: (M.Map Point Int, Octopuses) -> (M.Map Point Int, Octopuses)
next (keys, os) = (keys', fst <$> os')
    where
        keys' = (`M.restrictKeys` M.keysSet didn'tFlash) . freqs . concatMap allNeighbours . M.keys $ flashed
        (flashed, didn'tFlash) = M.partition snd os'
        os' = M.mapWithKey (boost keys) os
```

So we first map the octopuses `os` to a new set of octopuses `os'`.
We partition this into those that flashed and those that did not.
We then find the neighbours of the octopuses that flashed,
and turn that into a frequency map of how many times a neighbour octopus appears.

We then restrict that to octopuses that didn't flash, to ensure nothing flashes twice.

Then we return the octopuses to boost next time and the new octopuses.
If there are no more octopuses to flash the octopuses are unchanged and we will see an unchanged set of octopuses twice in a row, the fixed point.

We boost the energy as follows:

```haskell
boost :: Octopuses -> Point -> Int -> (Int, Bool)
boost ks k o | k `M.member` ks && o < 10 = let o' = o + lookupFreq k ks in (o', o' > 9)
             | otherwise   = (o, False)
```

If the octopus is in the set of octopuses to boost and the octopus has not yet flashed (`< 10`) then we boost it the required number of times
returning the new energy level and if it just flashed (`> 9`).
If not, we keep the level the same and return that it didn't _just_ flash.

We can then put this into a fixed point and reset the result:

```haskell
step :: Octopuses -> Octopuses
step os = Data.Map.map reset . snd $ fixedPoint next (1 <$ os, os)
    where
        reset o = if o > 9 then 0 else o
```

Here we initialise our frequency map to boost each octopus once, and pass the initial octopus state.
Then we get the final state of the octopuses out of the fixed point and reset each octopus.

That works by setting it's energy level to `0` if it was above `9`.

That's the core logic done.

For part 1 we count the total number of flashes after 100 days:

```haskell
solvea :: Octopuses -> Int
solvea os = (!! 100) . scanl1 (+) . map (countTrue (==0)) . iterate step $ os
```

To do this we keep running the step function, and for each step count the number of `0`s in the octopuses.
`scanl1 (+)` then generates a running total for each step, and finally we get the running total of day 100.

Job done!

For part 2 we need to keep going until all the octopuses flash at the same time, and return the step it happens on:

```haskell
solveb :: Octopuses -> Int
solveb os = fst . head . dropWhile (not . all (==0) . snd) . zip [0..] . iterate step $ os
```

We run the same core logic as last time of iterating the step function on the octopuses,
we then combine each state with it's step index (`zip [0..]`).

The we ignore each state where not all the octopuses have flashed (i.e. reset to `0`).
As soon as we find one (`head`) we take the step index and return it.

Part 2 complete!


*[Back to all reflections for 2021][reflections]*

## Day 11 Benchmarks

```
>> Day 11a
benchmarking...
time                 25.97 ms   (24.84 ms .. 26.81 ms)
                     0.994 R²   (0.985 R² .. 0.998 R²)
mean                 26.48 ms   (25.99 ms .. 27.47 ms)
std dev              1.333 ms   (710.4 μs .. 2.377 ms)
variance introduced by outliers: 16% (moderately inflated)

* parsing and formatting times excluded

>> Day 11b
benchmarking...
time                 120.9 ms   (103.9 ms .. 142.3 ms)
                     0.970 R²   (0.931 R² .. 0.999 R²)
mean                 110.4 ms   (107.4 ms .. 118.4 ms)
std dev              7.722 ms   (1.700 ms .. 11.59 ms)
variance introduced by outliers: 23% (moderately inflated)

* parsing and formatting times excluded
```

Day 6
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day06.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *6* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]* / *[17][day17]* / *[18][day18]* / *[25][day25]*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day01]: https://github.com/egnwd/advent/blob/2016/reflections-out/day01.md
[day02]: https://github.com/egnwd/advent/blob/2016/reflections-out/day02.md
[day03]: https://github.com/egnwd/advent/blob/2016/reflections-out/day03.md
[day04]: https://github.com/egnwd/advent/blob/2016/reflections-out/day04.md
[day05]: https://github.com/egnwd/advent/blob/2016/reflections-out/day05.md
[day07]: https://github.com/egnwd/advent/blob/2016/reflections-out/day07.md
[day08]: https://github.com/egnwd/advent/blob/2016/reflections-out/day08.md
[day09]: https://github.com/egnwd/advent/blob/2016/reflections-out/day09.md
[day10]: https://github.com/egnwd/advent/blob/2016/reflections-out/day10.md
[day11]: https://github.com/egnwd/advent/blob/2016/reflections-out/day11.md
[day12]: https://github.com/egnwd/advent/blob/2016/reflections-out/day12.md
[day13]: https://github.com/egnwd/advent/blob/2016/reflections-out/day13.md
[day14]: https://github.com/egnwd/advent/blob/2016/reflections-out/day14.md
[day15]: https://github.com/egnwd/advent/blob/2016/reflections-out/day15.md
[day16]: https://github.com/egnwd/advent/blob/2016/reflections-out/day16.md
[day17]: https://github.com/egnwd/advent/blob/2016/reflections-out/day17.md
[day18]: https://github.com/egnwd/advent/blob/2016/reflections-out/day18.md
[day25]: https://github.com/egnwd/advent/blob/2016/reflections-out/day25.md

*[Prompt][d06p]* / *[Code][d06g]*

[d06p]: https://adventofcode.com/2016/day/6
[d06g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day06.hs

For part 1 today I started with the most naïve approach where I simply generate all the fish and then count them,
fully aware this wouldn't scale, but it might get me the first star.

So for this approach we take a fish and generate a list of fish that come from this fish in the next day.

```haskell
step 0 = [6,8]
step f = [f-1]
```

next we map all of today's fish into their offspring, and join all the lists. Now we simply repeat the process until the nth day and see how many fish we have.

```haskell
solve = length . (!! n) . iterate (concatMap step)
```

Now fortuneately for only 80 days that didn't take to long, onto part 2.

First, I tried what I imagine most people did which was change the number to 256 and rerun.
I was sat there for a while... it didn't work.

In order to speed up the process we can notice something interesting:
if you have a fish that is on day N, over the course of 256 days it will have so many fish descendants,
following some family tree shape. (it's 4726100874 descendants, in case you are curious).

What is more is that if another fish starts on 4, then it will also have 4726100874 descendants.
So we can see that if there are N 4s in the input then we get `N * 4726100874` descendants.

As there are finitely many numbers that the fish can be represented by (0-8) we represent them not as a `[Int]` but as a very small `Map Int Int` where the key is the fish value and the value is the count of fish in that slot.

Everyday, there will be all the fish from the `0` slot making new fish, so we get `n` new fish in the `8` slot and `n` fish into the `6` "reset" slot. All the other fish shift down by one.

At the end of 256 days we can count the numbers in the map to see how many fish there are.

```haskell
solve :: Int -> [Fish] -> Int
solve n = sum . (!! n) . iterate step . freqs

step :: Map Fish Int -> Map Fish Int
step m = M.insertWith (+) 6 newFish . M.mapKeys tickDown $ m
    where
        newFish = m ! 0
        tickDown = maybe 8 weaken . unshift
```

_`maybe 8 weaken . unshift` represents wrapping the numbers around, 6 -> 5, 2 -> 1, and 0 -> 8 (as `unshift 0` will return `Nothing`, so we reset it to 8)_

To me it makes more sense to think about the fish resetting to `8` and new fish coming in at `6`, just so the wrapping and insertion keeps the fish the same... but that's just me.


*[Back to all reflections for 2016][reflections]*

## Day 6 Benchmarks

```
[ERROR]
Day not yet avaiable: 6
```

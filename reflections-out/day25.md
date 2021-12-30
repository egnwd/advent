Day 25
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day25.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]* / *[17][day17]* / *[18][day18]* / *25*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day01]: https://github.com/egnwd/advent/blob/2016/reflections-out/day01.md
[day02]: https://github.com/egnwd/advent/blob/2016/reflections-out/day02.md
[day03]: https://github.com/egnwd/advent/blob/2016/reflections-out/day03.md
[day04]: https://github.com/egnwd/advent/blob/2016/reflections-out/day04.md
[day05]: https://github.com/egnwd/advent/blob/2016/reflections-out/day05.md
[day06]: https://github.com/egnwd/advent/blob/2016/reflections-out/day06.md
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

*[Prompt][d25p]* / *[Code][d25g]*

[d25p]: https://adventofcode.com/2016/day/25
[d25g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day25.hs

Christmas Day!

Another iteration based approach, just needing the index of the fixed point.

Let's start by defining a stepping function for the sea cucumbers.

```haskell
step :: Point -> Map Point Dir -> Map Point Dir
step bounds = stepDir South . stepDir East
    where
        stepDir :: Dir -> Map Point Dir -> Map Point Dir
        stepDir dir mp = mapKeys moveCuke mp
            where
                dirs = keysSet . filter (==dir) $ mp
                moveCuke p = let p' = mod <$> p + dirVec dir <*> bounds
                                 isDir = member p dirs
                              in if isDir && p' `M.notMember` mp then p' else p
```

We define the landscape of sea cucumbers as a map from it's location to which direction it's facing.
To step the cucumbers we first move all the East pointing cucumbers, then the south facing ones.
And so to step the cucumbers in a direction we map all the locations to a new location,
if the cucumber isn't facing in the direction of interest we don't move it, if it is in the direction of interest,
and there is no cucumber in the new location, we set it's location to the new location.
We calculate the new location by adding the direction vector to the original point and taking that modulo the bounds of the map.
_N.B. East -> `V2 1 0` and South -> `V2 0 1`_

We then just need to find the fixed point:

```haskell
solve mp = go 1 mp
  where
    go idx !x
        | x == y    = idx
        | otherwise = go (idx+1) y
      where
        y = step (findMax mp) x
```

Christmas saved! Merry Christmas!!


*[Back to all reflections for 2016][reflections]*

## Day 25 Benchmarks

```
[ERROR]
Day not yet avaiable: 25
```

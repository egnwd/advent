Day 5
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day05.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *5* / *[6][day06]*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day01]: https://github.com/egnwd/advent/blob/2021/reflections-out/day01.md
[day02]: https://github.com/egnwd/advent/blob/2021/reflections-out/day02.md
[day03]: https://github.com/egnwd/advent/blob/2021/reflections-out/day03.md
[day04]: https://github.com/egnwd/advent/blob/2021/reflections-out/day04.md
[day06]: https://github.com/egnwd/advent/blob/2021/reflections-out/day06.md

*[Prompt][d05p]* / *[Code][d05g]*

[d05p]: https://adventofcode.com/2021/day/5
[d05g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day05.hs

Lots of 2D grids!

Another day we get to use the `Linear` package. We can model a line as a 2D vector of points (`V2 (V2 Int)`).

For part 1 we are told to ignore diagonal lines (I wonder what part 2 could possibly be!) so we can filter those easily enough:

```haskell
isHorizontalOrVertical :: V2 (V2 Int) -> Bool
isHorizontalOrVertical (V2 (V2 x1 y1) (V2 x2 y2)) = x1 == x2 || y1 == y2

horizontalOrVerticalLines :: [V2 (V2 Int)] -> [V2 (V2 Int)]
horizontalOrVerticalLines lines = filter isHorizontalOrVertical lines
```

Next, we need to generate the full lines. If we take 2 points `a` & `b` and find their difference `b - a`,
then we can get the vector between them. Reducing this vector to it's simplest form by dividing each component by the greatest common divisor
of the components will give us the smallest possible hops we can make to hit a valid grid point.
We then make little hops and collect the points in a list.

This is a modified version of [mstksg's](https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Common/Point.hs#L371) `lineTo` function from last years AOC.

```haskell
lineTo :: V2 Point -> [Point]
lineTo (V2 p0 p1) = [p0 + t *^ step | t <- [0 .. gcf]]
  where
    d@(V2 dx dy) = p1 - p0
    gcf          = gcd dx dy
    step         = (`div` gcf) <$> d
```

and as the tooltip hinted, we are looking along the lines of plotting lines so the [Bresenham's line algorithm](https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm#Algorithm_for_integer_arithmetic) is a nice read, though not necessarily useful for the problem other than providing intuition.

Then all we need to do find the frequency of getting all the points from all the line segments:

```haskell
pointFrequencies lines = fromMapWith (+) . map (,1) . concatMap lineTo $ lines
```

Then finally find the number of points that are visited by more than 1 line segment.

```haskell
solve lines = size . filter (>1) . pointFrequencies $ lines
```

Done! 1 star!

As suspected, part 2 removes the horizontal/vertical line restriction, and so we need simply remove our filter.

Hydrothermal vent field successfully navigated!


*[Back to all reflections for 2021][reflections]*

## Day 5 Benchmarks

```
>> Day 05a
benchmarking...
time                 222.4 ms   (211.9 ms .. 236.0 ms)
                     0.998 R²   (0.994 R² .. 1.000 R²)
mean                 218.0 ms   (214.7 ms .. 222.0 ms)
std dev              5.663 ms   (3.912 ms .. 7.234 ms)
variance introduced by outliers: 14% (moderately inflated)

* parsing and formatting times excluded

>> Day 05b
benchmarking...
time                 381.1 ms   (370.1 ms .. 399.2 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 377.9 ms   (371.5 ms .. 381.7 ms)
std dev              6.356 ms   (2.334 ms .. 8.720 ms)
variance introduced by outliers: 19% (moderately inflated)

* parsing and formatting times excluded
```

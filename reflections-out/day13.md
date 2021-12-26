Day 13
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day13.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *13* / *[14][day14]* / *[15][day15]*

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
[day11]: https://github.com/egnwd/advent/blob/2021/reflections-out/day11.md
[day12]: https://github.com/egnwd/advent/blob/2021/reflections-out/day12.md
[day14]: https://github.com/egnwd/advent/blob/2021/reflections-out/day14.md
[day15]: https://github.com/egnwd/advent/blob/2021/reflections-out/day15.md

*[Prompt][d13p]* / *[Code][d13g]*

[d13p]: https://adventofcode.com/2021/day/13
[d13g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day13.hs

Today was interesting, and our first time this year seeing a non-Int answer.

For our first task we need to parse the data.

I chose to represent the paper as a set of points (`Set (V2 Int)`) and the folds as a custom data type (`Fold`).

```haskell
type TransparentPaper = Set (V2 Int)
data Fold = FX Int | FY Int
```

_`V2` is from the `Linear` library and represents a vector of 2 items._

For details on the full parsing check out the code but the main idea is:
 - For a point split on the ',' and make a `V2` from the two numbers
 - Turn all the points into a set
 - For a fold, if it's `"x="` then parse the number into the `FX` constructor, else parse it into the `FY` constructor

and that's pretty much it!

So for part 1 we just need to apply the first fold and count the remaining points,
nicely alluding to what part 2 is going to be and making sure we are on the right track.

To start with I needed a function that decides if a point is beyond the fold or not.
From the problem we know that points that get moved are "greater" than the fold line.

So we can define a `beyondFold` function like so:

```haskell
beyondFold :: V2 Int -> Fold -> Bool
beyondFold (V2 x y) = \case
    FX fx -> x > fx
    FY fy -> y > fy
```

next we need to remap any points that lie beyond the fold to new positions on the folded side.

```
┌─────┐
│ #   │
│ ▲   │
├─┼───┤
│ │   │
│ #   │
└─────┘
```

in this example the **#** is being mapped to above the line `y=2`.

This results in a transformation from `(1,4)` to `(1,0)`.

More generally we can get this by, for a Y-fold, keeping `x` the same,
and then subtracting from the line (`2`) the distance from the point to the line (`4-2`).
`fy - (y - fy)` which simplifies to `2 * fy - y`.
Similar logic can be used for the X-fold, replacing `y` & `fy` for `x` & `fx`, respectively.

This gives us our remapPoint function:

```haskell
remapPoint :: Point -> Fold -> Point
remapPoint (V2 x y) = \case
    FX fx -> V2 (2*fx-x) y
    FY fy -> V2 x        (2*fy-y)
```

The only remaining thing to do is partition the points, remap the ones beyond the fold and union the remapped points with the ones that didn't move.

```haskell
applyFold :: TransparentPaper -> Fold -> TransparentPaper
applyFold points f = union remappedPoints remainingPoints
    where
        remappedPoints = map (`remapPoint` f) pointsToMove
        (pointsToMove, remainingPoints) = partition (`beyondFold` f) points
```

_You could also do this with a single `map` with an `if`, but I preferred this approach._

Finally, we just need to count the number of points after the first fold:

```haskell
solve paper (f:fs) = size $ applyFold paper f
```

Woo, part 1 done!

For part 2 we need to apply all the folds and read the string from the ASCII art.

Applying all the folds is simple, we use:

```haskell
solve paper folds = foldl applyFold paper folds
```

This results in a set of points that we can print, squint and type into the answer box.

_However, that doesn't get us the final answer..._

To do this I used the [`Advent.OCR` library](https://github.com/mstksg/advent-of-code-ocr),
which exposes a `parseLettersWith` function which takes a function for extracting the x coordinate, one for the y coordinate and a set of points.

```haskell
readCode paper = parseLettersWith (view _x) (view _y) paper
```

Now we are ready to do some thermal imaging!


*[Back to all reflections for 2021][reflections]*

## Day 13 Benchmarks

```
>> Day 13a
benchmarking...
time                 9.140 ms   (8.989 ms .. 9.345 ms)
                     0.997 R²   (0.994 R² .. 0.998 R²)
mean                 9.202 ms   (9.044 ms .. 9.348 ms)
std dev              409.6 μs   (306.2 μs .. 579.2 μs)
variance introduced by outliers: 18% (moderately inflated)

>> Day 13b
benchmarking...
time                 6.140 ms   (5.671 ms .. 6.795 ms)
                     0.957 R²   (0.915 R² .. 0.998 R²)
mean                 5.750 ms   (5.637 ms .. 6.009 ms)
std dev              507.0 μs   (187.8 μs .. 935.5 μs)
variance introduced by outliers: 54% (severely inflated)
```

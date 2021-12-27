Day 17
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day17.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]* / *17* / *[18][day18]*

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
[day13]: https://github.com/egnwd/advent/blob/2021/reflections-out/day13.md
[day14]: https://github.com/egnwd/advent/blob/2021/reflections-out/day14.md
[day15]: https://github.com/egnwd/advent/blob/2021/reflections-out/day15.md
[day16]: https://github.com/egnwd/advent/blob/2021/reflections-out/day16.md
[day18]: https://github.com/egnwd/advent/blob/2021/reflections-out/day18.md

*[Prompt][d17p]* / *[Code][d17g]*

[d17p]: https://adventofcode.com/2021/day/17
[d17g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day17.hs

My day 17 is not the fastest thing in the world, but it's a fairly neat and readable solution this time.

Parsing the problem is quite nice, and we can take advantage of `V2` again and it's nice Distributive properties.
`Linear` exposes this as the `transpose` function.

```haskell
type Region = V2 (V2 Int)

parser :: CharParser Region
parser = transpose <$> (V2 <$> ("target area: " *> "x=" *> parseRange <* ", ") <*> ("y=" *> parseRange))
    where
        parseRange = V2 <$> pDecimal <*> (".." *> pDecimal)
```

We parse the x range into a `V2` and the y range into a `V2`, then transpose the little matrix:
For example: given `target area: x=0..100, y=10..60` we parse it into `V2 (V2 0 100) (V2 10 60)`,
then `transpose` gives us `V2 (V2 0 10) (V2 100 60)`, i.e. the minimum corner and the maximum corner of the target area.

**Part 1:**

For part 1 we can simulate various paths for the trajectories, up until we have gone past the target area.
After that we can check if it's a valid trajectory if it is in the bounding box.
From the trajectories that remain we can find the maximum Y displacement.

Let's first start by defining a function that checks if a point is past a bounding box.

```haskell
pastBoundingBox
    :: (Integral a)
    => V2 (V2 a) -- Bounding Box
    -> V2 a      -- Point to check
    -> Bool
pastBoundingBox b p = or $ go <$> p <*> maxB
  where
    V2 xs ys = transpose b
    maxB = floor <$> maximumBy (dist @Double) [V2 (fromIntegral x) (fromIntegral y) | x <- toList xs, y <- toList ys]
    dist :: forall a. (Floating a, Ord a) => V2 a -> V2 a -> Ordering
    dist = compare `on` distanceA zero
    go x' mx' = if mx' < 0 then x' < mx' else x' > mx'
```

`maxB` finds the corner furthest from the origin, we then compare the coordinates of the target point to the calculated maximum point, and if either coordinate is larger (or smaller if negative) then we return that it's past the bounding box.

Next we need some functions to calculate the points on a trajectory path:

```haskell
type Point = V2 Int
type Velocity = V2 Int

positions :: Point -> Velocity -> [Point]
positions p0 = scanl (+) p0 . velocities

velocities :: Velocity -> [Velocity]
velocities = iterate ((_x %~ dragX) . (_y %~ dragY))
    where
        dragX vx = vx - signum vx
        dragY vy = vy - 1
```

`velocities` takes a starting velocity and returns all the decaying velocities by iterating a decay function.
First we decay the `y` component of the velocity, by deducting 1. Then we decay the `x` component by moving it 1 towards 0,
this is the same as deducting the sign of the velocity from the velocity (_note `signum` returns `1` for positive numbers, `-1` for negative numbers, and `0` for `0`_).

`positions` simply uses `scanl` to get a running cumulative sum of the velocities starting from some origin `p0`.

We then truncate the trajectory when we get past the target region, starting from (0,0).

```haskell
trajectory :: Region -> Velocity -> [Point]
trajectory targ = takeWhile (not . pastBoundingBox targ) . positions zero
```

Next we define a function that find's if a point is in the target region:

```haskell
findCollision :: Region -> [Point] -> Maybe Point
findCollision targ = find (inBoundingBox targ)
```

The last thing we need to do for part 1 is put all this together to find the maximum point.

```haskell
highestY :: Region -> Int
highestY targ
  = getMax
  . fold
  . mapMaybe ((\t -> findMaxHeight t <$ findCollision targ t) . trajectory targ)
  . filter ((>0) . view _x)
  $ validRange targ
    where
        findMaxHeight = foldMap (Max . view _y)
```

We generate a valid range of starting velocites, just to limit the search space.
For each starting velocity we generate the trajectory, attempt to find a collision,
and if we do the the max height of the trajectory `t`.
_`b <$ fa` is the same as `const a <$> fa`, where `<$>` is just infix `fmap`._

The final `fold` finds the `Max` of all the `Maxes`.

Part 1 done, and the trick shot looked pretty cool.

For part 2 we need to find all options.

Given all our helper functions, we have a very easy mechanism to do this:

```haskell
numberVelocities :: Region -> Int
numberVelocities targ = length . mapMaybe (findCollision targ . trajectory targ) $ validRange targ
```

We just omit calculating the max height and count the number of valid collisions we find.

Part 2 done!


*[Back to all reflections for 2021][reflections]*

## Day 17 Benchmarks

```
>> Day 17a
benchmarking...
time                 396.7 ms   (350.5 ms .. 444.6 ms)
                     0.997 R²   (0.997 R² .. 1.000 R²)
mean                 415.6 ms   (404.4 ms .. 423.7 ms)
std dev              11.62 ms   (5.644 ms .. 16.26 ms)
variance introduced by outliers: 19% (moderately inflated)

* parsing and formatting times excluded

>> Day 17b
benchmarking...
time                 319.2 ms   (254.2 ms .. 391.5 ms)
                     0.991 R²   (0.990 R² .. 1.000 R²)
mean                 413.6 ms   (370.0 ms .. 492.9 ms)
std dev              76.42 ms   (3.305 ms .. 93.54 ms)
variance introduced by outliers: 47% (moderately inflated)

* parsing and formatting times excluded
```

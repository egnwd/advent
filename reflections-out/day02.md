Day 2
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day02.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *2*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day01]: https://github.com/egnwd/advent/blob/2021/reflections-out/day01.md

*[Prompt][d02p]* / *[Code][d02g]*

[d02p]: https://adventofcode.com/2021/day/2
[d02g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day02.hs

Moving around a 2D map, another AoC classic challenge.

My initial solution used recursion and word splitting for the solution,
but I have an affinity for parsing and the state monad, so that's what I'll describe here.

To parse we can read many tuples of `(Direction, Distance)` and have that as our instruction set.
The parser combinators make this quite nice to do:

```haskell
parser = do
  dir <- pTok $ (Forward <$ "forward") <|> (Up <$ "up") <|> (Down <$ "down")
  dist <- pDecimal
  return (dir, dist)
```

Then to solve part 1 we just need to move through the instructions, summing the scaled vectors together.
I used the `V2` type from `Linear.V2` to represent points as they are 2D vectors with the added bonus of
being able to easily to component-wise addition of vectors.


To get the vectors:

```haskell
positionUpdate = \case
    Forward -> V2 1 0
    Up      -> V2 0 (-1)
    Down    -> V2 0 1
```

```haskell
solve = product . sum . map (\(d,n) -> pure n * positionUpdate d)
```

After the submarine's epic journey, we get 1 star!

For part two, I used the State monad.
You can quite easily use a fold, which I did initially but it was quite messy, I felt this was easier to read.

To update the state we have the following, for each instruction, update the location or aim using:
```haskell
moveSubmarine Forward n = do
        a <- use aim
        loc += V2 n (n*a)
moveSubmarine d n = aim += n * aimUpdate d
    where
        aimUpdate = \case
            Up   -> -1
            Down -> 1
            _    -> 0
```

then we just extract the final `loc` and get the `product` like last time.

Submarine successfully piloted!


*[Back to all reflections for 2021][reflections]*

## Day 2 Benchmarks

```
>> Day 02a
benchmarking...
time                 3.811 ms   (3.759 ms .. 3.889 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 3.781 ms   (3.721 ms .. 3.821 ms)
std dev              164.2 μs   (126.4 μs .. 240.8 μs)
variance introduced by outliers: 25% (moderately inflated)

>> Day 02b
benchmarking...
time                 2.332 ms   (2.310 ms .. 2.357 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.345 ms   (2.330 ms .. 2.363 ms)
std dev              54.61 μs   (42.35 μs .. 69.57 μs)
variance introduced by outliers: 10% (moderately inflated)
```

Day 2
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day02.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *2* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day01]: https://github.com/egnwd/advent/blob/2021/reflections-out/day01.md
[day03]: https://github.com/egnwd/advent/blob/2021/reflections-out/day03.md
[day04]: https://github.com/egnwd/advent/blob/2021/reflections-out/day04.md
[day05]: https://github.com/egnwd/advent/blob/2021/reflections-out/day05.md
[day06]: https://github.com/egnwd/advent/blob/2021/reflections-out/day06.md
[day07]: https://github.com/egnwd/advent/blob/2021/reflections-out/day07.md
[day08]: https://github.com/egnwd/advent/blob/2021/reflections-out/day08.md
[day09]: https://github.com/egnwd/advent/blob/2021/reflections-out/day09.md

*[Prompt][d02p]* / *[Code][d02g]*

[d02p]: https://adventofcode.com/2021/day/2
[d02g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day02.hs

Moving around a 2D map, another AoC classic challenge.

My initial solution used recursion and word splitting for the solution,
the second used State, and that too was more difficult to understand, and felt more imperative.

The version here uses Monoids to combine the values (part a ended up similar to my first fold attempt).

To parse we can read many `Sum`mable `Points` and have that as our instruction set.
The parser combinators make this quite nice to do:

```haskell
parser = do
  dir <- pTok $
        (Sum . flip V2 0     <$ "forward")
    <|> (Sum . V2 0 . negate <$ "up")
    <|> (Sum . V2 0          <$ "down")
  dir <$> pDecimal
```

Then to solve part 1 we just need to combine the instructions.
I used the `V2` type from `Linear.V2` to represent points as they are 2D vectors with the added bonus of
being able to easily to component-wise addition of vectors.

```haskell
solve = product . getSum . mconcat
```

After the submarine's epic journey, we get 1 star!

For part two, I wrote a new type `Submarine` with a location and aim.

```haskell
data Submarine = Sub { loc :: Point, aim :: Int }
```

this time to combine we must implement `Monoid`, and first Semigroup.

```haskell
instance Semigroup Submarine where
    (Sub v a) <> (Sub (V2 x' y') a') = Sub (v + V2 x' (y' + x' * a)) (a + a')

instance Monoid Submarine where
    mempty = Sub (pure 0) 0
    mconcat = foldl (<>) mempty
```

`(<>)` combines two commands, adding the forward components `x+x'`,
updates the depth taking into account the previous aim and the forward distance
`y + y' + x' * a` (for the `up` & `down` commands `x'` & `y'` are `0` so that term cancels),
and finally the aim is updated (for `forward` this is `0` so is also a noop).

Then to combine we do similar to part 1:
```haskell
solve = product . loc . mconcat
```

where `loc` gets the vector from the `Submarine` akin to `getSum` getting the vector from `Sum`.

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

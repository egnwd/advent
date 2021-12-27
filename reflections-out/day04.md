Day 4
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day04.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *4* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]* / *[17][day17]* / *[18][day18]* / *[25][day25]*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day01]: https://github.com/egnwd/advent/blob/2021/reflections-out/day01.md
[day02]: https://github.com/egnwd/advent/blob/2021/reflections-out/day02.md
[day03]: https://github.com/egnwd/advent/blob/2021/reflections-out/day03.md
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
[day17]: https://github.com/egnwd/advent/blob/2021/reflections-out/day17.md
[day18]: https://github.com/egnwd/advent/blob/2021/reflections-out/day18.md
[day25]: https://github.com/egnwd/advent/blob/2021/reflections-out/day25.md

*[Prompt][d04p]* / *[Code][d04g]*

[d04p]: https://adventofcode.com/2021/day/4
[d04g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day04.hs

Bingo against a giant squid is a funny idea 😆

Today we got to learn about two Semigroups! `First` and `Last`,
they are fairly self explanatory, `First` will give you the left (first) argument to `(<>)`
and `Last` will give you the second (last) argument to `(<>)`.

_N.B. `Last` is the `Dual` of `First`._

To parse we need two things, the list of numbers and all the cards.

```haskell
data BingoNumber = Unmarked Int | Marked Int deriving (Eq, Show)
type BingoCard = [[BingoNumber]]

parse :: String -> ([Int], [BingoCard])
```

Here we can represent a number as either marked or unmarked (note this is just `Either` with a nicer name).

Then we can define our solve function to take the list of numbers, the boards, and return the score, maybe.

```haskell
solve :: [Int] -> [BingoCard] -> Maybe Score
```

(We will look at `Score` later.)

Then as we see a number, `n`, we mark all our cards.

```haskell
newCards n cards = cards'
  where
    cards' = cards <&> mapped.mapped %~ mark
    mark (Unmarked x) | x == n = Marked x
    mark x = x
```

We can define what it means for a card to have won:

```haskell
winner :: BingoCard -> Bool
winner cards = row cards || column cards
    where
        column = row . transpose
        row = any (all isMarked)

isMarked :: BingoNumber -> Bool
isMarked = \case
    Marked _ -> True
    _ -> False
```

Next, we work out the scores for all the winners this round.

```haskell
scoreWinners cards' n = winners
  where
      winners = foldMap (Just . First . scoreCard) w
      w = filter winner cards'
      scoreCard card = (sum . unmarked $ card) * n
      unmarked = map unmarkedValue . concat

unmarkedValue :: BingoNumber -> Int
unmarkedValue (Unmarked n) = n
unmarkedValue (Marked _)   = 0
```

and here is where we get to see `First`.

We've taken our cards and found the winners, then we score them using the score system from the problem statement,
and next we make a `Maybe (First Int)` with `Just . First`. This represents (the maybe is to represent the fact we have a score).

`foldMap` will the return `Nothing` if there are no winners this round, or `Just (First x)` where `x` is the score of the first winner.

We then recurse on the rest of the numbers and the boards that haven't won yet.

```haskell
solve (n:ns) cards = scoreWinners cards' n <> solve ns (filter (not . winner) cards')
  where
    cards' = newCards n cards
```

Now for part two all we need to do is swap `First` for `Last`!!

Now, hopefully the squid doesn't eat us.


*[Back to all reflections for 2021][reflections]*

## Day 4 Benchmarks

```
>> Day 04a
benchmarking...
time                 15.18 ms   (14.92 ms .. 15.40 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 15.10 ms   (14.94 ms .. 15.24 ms)
std dev              380.8 μs   (270.1 μs .. 579.1 μs)

* parsing and formatting times excluded

>> Day 04b
benchmarking...
time                 20.12 ms   (19.95 ms .. 20.35 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 20.05 ms   (19.94 ms .. 20.14 ms)
std dev              231.6 μs   (169.9 μs .. 321.9 μs)

* parsing and formatting times excluded
```

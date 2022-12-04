Day 2
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day02.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *2* / *[3][day03]* / *[4][day04]*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day01]: https://github.com/egnwd/advent/blob/2022/reflections-out/day01.md
[day03]: https://github.com/egnwd/advent/blob/2022/reflections-out/day03.md
[day04]: https://github.com/egnwd/advent/blob/2022/reflections-out/day04.md

*[Prompt][d02p]* / *[Code][d02g]*

[d02p]: https://adventofcode.com/2022/day/2
[d02g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day02.hs

Hmm... Well I messed up.

I had the solution fairly quickly by hard coding the mappings suggested and lots of lookups...
and then decided the intended solution was to find the *maximum* score given any strategy...

which does have a nice little form

```haskell
strategies = map (zip "ABC") (permutations "XYZ")
```

and then you simply need to find the maximum from applying all the strategies.


But that's wrong, so let's simplify.

First we can set up a parser, using the default lookup:

```haskell
parseGame = do
    opp <- Rock <$ "A" <|> Paper <$ "B" <|> Scissors <$ "C"
    pSpace
    you <- Rock <$ "X" <|> Paper <$ "Y" <|> Scissors <$ "Z"
    return (opp, you)
```

and then we can score a game given some rules on the winner:

```haskell
Paper    `play` Rock     = Win
Rock     `play` Scissors = Win
Scissors `play` Paper    = Win
a        `play` b
  | a == b = Draw
  | otherwise = Lose
```

I wasn't hugely happy with this, I tried an implementation with `Ord` and another with `Map`
but this was still the simplest, and it's a total function as we've parsed the inputs into nice types.

Then given the scoring functions set out in the puzzle:

```haskell
score you out = scoreRPS you + scoreOutcome out

scoreRPS = \case
    Rock     -> 1
    Paper    -> 2
    Scissors -> 3

scoreOutcome = \case
    Win  -> 6
    Draw -> 3
    Lose -> 0
```

we can score a game simply using ``\opp you -> score you (you `play` opp)``.

Finally.

Then for part two, much the same we just need a different lookup and different parser.

```haskell
parseStrategy = do
    opp <- Rock <$ "A" <|> Paper <$ "B" <|> Scissors <$ "C"
    pSpace
    out <- Lose <$ "X" <|> Draw <$ "Y" <|> Win <$ "Z"
    return (opp, out)

whatToPlay a        Draw = a
whatToPlay Paper    Win  = Scissors
whatToPlay Rock     Win  = Paper
whatToPlay Scissors Win  = Rock
whatToPlay Paper    Lose = Rock
whatToPlay Rock     Lose = Scissors
whatToPlay Scissors Lose = Paper
```

again, I'm not a fan of huge lookups but there we go.

putting together what we already have gives us ``\opp out -> score (whatToPlay opp out) out``.

Two stars, but not the most satisfying.


*[Back to all reflections for 2022][reflections]*

## Day 2 Benchmarks

```
>> Day 02a
benchmarking...
time                 5.104 ms   (5.050 ms .. 5.174 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 5.089 ms   (5.033 ms .. 5.130 ms)
std dev              142.0 μs   (113.0 μs .. 190.2 μs)
variance introduced by outliers: 11% (moderately inflated)

>> Day 02b
benchmarking...
time                 2.760 ms   (2.734 ms .. 2.813 ms)
                     0.987 R²   (0.962 R² .. 0.999 R²)
mean                 2.812 ms   (2.771 ms .. 3.004 ms)
std dev              237.1 μs   (88.73 μs .. 499.2 μs)
variance introduced by outliers: 57% (severely inflated)
```

Day 14
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day14.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *14* / *[15][day15]* / *[16][day16]* / *[17][day17]* / *[18][day18]* / *[25][day25]*

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
[day15]: https://github.com/egnwd/advent/blob/2016/reflections-out/day15.md
[day16]: https://github.com/egnwd/advent/blob/2016/reflections-out/day16.md
[day17]: https://github.com/egnwd/advent/blob/2016/reflections-out/day17.md
[day18]: https://github.com/egnwd/advent/blob/2016/reflections-out/day18.md
[day25]: https://github.com/egnwd/advent/blob/2016/reflections-out/day25.md

*[Prompt][d14p]* / *[Code][d14g]*

[d14p]: https://adventofcode.com/2016/day/14
[d14g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day14.hs

Another growth issue to contend with, but we can solve it in a similar manner to the lanternfish,
keeping track of a frequency map of states to reduce the number of operations we need to perform.

Luckily part 1 & 2 have common solution.

First let's set up the skeleton of the process, we need to input a formula and get the range of frequencies out.

```haskell
type Element = Char

formulaFrequencies :: Map (Element, Element) Int -> Map Element Int
formulaFrequencies = M.mapKeysWith (+) snd

scoreFormula :: (Num n, Ord n) => Map a n -> Maybe n
scoreFormula f
  = case (maximumOf traverse f, minimumOf traverse f) of
    (Just mx, Just mn) -> Just (mx - mn)
    _                    -> Nothing

solve :: Int -> (String, Map (Element, Element) Element) -> Maybe Int
solve n = scoreFormula . formulaFrequencies <=< uncurry (generateFormula n)
```

`generateFormula` will take an initial formula and give back a map of element pair frequencies after `n` steps.
`formulaFrequencies` take the steps and returns just the second element from each pair.
This is to avoid repeated counts for the elements.
_I'm pretty sure this nly works because the initial letter of my input formular is neither the minimum or maximum, but oh well._

`scoreFormula` then gets the range of the frequencies, assuming they exist.

The core then happens in `generateFormula`.

We iterate a substituion step n times and return the final frequencies.

```haskell
generateFormula :: Int -> String -> FormulaMapping -> Maybe (FormulaFrequencies (Element, Element))
generateFormula n formula m = (!? n) . iterate substitute $ formula'
    where
        formula' = freqs . pairwise $ formula
        pairwise ls = zip ls (tail ls)
        substitute = M.fromListWith (+) . foldMap (\(k,a) -> map (,a) . new $ k) . M.toList
        new k@(a,b) = case M.lookup k m of { Just c -> [(a,c), (c,b)]; Nothing -> [k] }
```

The `substitute` step takes each pair and runs a substituion phase which produces up to 2 pairs,
for example given the pair `(NV, 4)` and the substituion `NV -> C` we get `[(NC, 4), (CV, 4)]`.

We then roll up the frequencies into another frequency map.

`n` steps later and we have the answer!


*[Back to all reflections for 2016][reflections]*

## Day 14 Benchmarks

```
[ERROR]
Day not yet avaiable: 14
```

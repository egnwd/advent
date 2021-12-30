Day 10
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day10.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *10* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *[16][day16]* / *[17][day17]* / *[18][day18]* / *[25][day25]*

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
[day11]: https://github.com/egnwd/advent/blob/2016/reflections-out/day11.md
[day12]: https://github.com/egnwd/advent/blob/2016/reflections-out/day12.md
[day13]: https://github.com/egnwd/advent/blob/2016/reflections-out/day13.md
[day14]: https://github.com/egnwd/advent/blob/2016/reflections-out/day14.md
[day15]: https://github.com/egnwd/advent/blob/2016/reflections-out/day15.md
[day16]: https://github.com/egnwd/advent/blob/2016/reflections-out/day16.md
[day17]: https://github.com/egnwd/advent/blob/2016/reflections-out/day17.md
[day18]: https://github.com/egnwd/advent/blob/2016/reflections-out/day18.md
[day25]: https://github.com/egnwd/advent/blob/2016/reflections-out/day25.md

*[Prompt][d10p]* / *[Code][d10g]*

[d10p]: https://adventofcode.com/2016/day/10
[d10g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day10.hs

For Day 10 I kept it simple and didn't do anything _too_ fancy.

Parsing is straight forward, we just split the lines.

For part 1 we are looking for the sum of the scores of the incorrect brackets.

We can do this by keeping track of a stack of open brackets,
then breaking when we hit a closing bracket that mis-matches.

```haskell
solvea :: String -> Maybe (Sum Integer)
solvea [] = Nothing
solvea (b:bs) = solve' bs [b]
    where
        solve' [] _ = Nothing
        solve' (_:_) [] = Nothing
        solve' (b':bs') (o:os) | b' `elem` "({[<"                       = solve' bs' (b':o:os)
                               | [o,b'] `elem` ["()", "[]", "{}", "<>"] = solve' bs' os
                               | otherwise                              = Just $ scorea b'
```

`Nothing` means that there was no incorrect bracket,
`Just` returns the score of the incorrect bracket as a `Sum`.

The logic of `solve'` returns `Nothing` if either of our tracking lists is empty.
Otherwise, we have one of the following conditions:

1. if we encounter an opening bracket: we push it onto the stack and recurse on the rest of the list.
1. otherwise we have a closing bracket, but if it's matching:
we recurse on the rest of the list, removing the opening bracket from the stack.
1. finally, we have the case where we have an incorrect bracket: we score the bracket and return it.

The score function is a simple lookup:

```haskell
scorea :: Char -> Sum Integer
scorea = \case
    ')' -> Sum 3
    ']' -> Sum 57
    '}' -> Sum 1197
    '>' -> Sum 25137
    _   -> mempty
```

To get the score of everything we can then aggregate the scores we get from all the lines:

```haskell
solve bs = getSum <$> foldMap solvea bs
```

_N.B. `Maybe (Sum Int)` works nicely with the `fold`, `Nothing <> Just (Sum 1)` is simply `Just (Sum 1)`.
`Nothing <> Nothing` is `Nothing` and `Just (Sum 4) <> Just (Sum 3)` is `Just (Sum 7)`.
So we nicely skip the rows that don't have the mismatching property._

We get our answer and move onto part 2.

For part two we can do something very similar with two changes: what we are looking for is different and the scoring is different.

```haskell
solveb :: String -> Maybe Integer
solveb [] = Nothing
solveb (b:bs) = solve' bs [b]
    where
        solve' _ [] = Nothing
        solve' [] os = Just . autoScore . mconcat . map scoreb $ os
        solve' (b':bs') (o:os) | b' `elem` "({[<"                       = solve' bs' (b':o:os)
                               | [o,b'] `elem` ["()", "[]", "{}", "<>"] = solve' bs' os
                               | otherwise                              = Nothing
```

Like last time if we finish with `Nothing` if the row isn't of the right type and `Just` the score if we are left with unclosed brackets.

This time, if the opening brackets is empty we return nothing, but we calculate the score if the string is depleted with brackets left in the open brackets stack.
I'll go through the scoring logic later.
If both lists have elements we once again shift open brackets to the stack,
recurse on matching brackets popping the open bracket from the stack.
If it's a mismatching bracket we are in the part 1 case and so return `Nothing`.

For the scoring we need to multiply the current score by 5 and add the next score.

_N.B. This is just base 5 as the scores are 1-4._

However, as I'd just `Sum` in part 1 I wanted to try a Monoid for part 2.

```haskell
data AutocompleteScore a = (Num a) => Auto { autoScore :: a }

instance Semigroup (AutocompleteScore a) where
    (Auto s) <> (Auto s') = Auto ((s * 5) + s')

instance (Num a) => Monoid (AutocompleteScore a) where
    mempty = Auto 0
    mconcat = foldl (<>) mempty
```

It works by taking the left side of the `(<>)` multiplying by 5, then adding the right side.
The `mempty` is just `0`, and `mconcat` is a `foldl` not `foldr` like the default implementation.
_Technically, this isn't a Semigroup as it doesn't satisfy the Associativity law... but I didn't mind for this use case._

```haskell
scoreb :: Char -> AutocompleteScore Integer
scoreb = \case
    '(' -> Auto 1
    '[' -> Auto 2
    '{' -> Auto 3
    '<' -> Auto 4
    _   -> mempty
```

_N.B. We don't need to map to the closing bracket and score that like the problem states, scoring the open bracket works just as well._

Finally, we just need to pic the middle number, which is easy enough with:

```haskell
middle :: (Ord a) => [a] -> Maybe a
middle ns = sort ns !? (length ns `div` 2)

solve bs = middle . mapMaybe solveb $ bs
```

Part 2 done!


*[Back to all reflections for 2016][reflections]*

## Day 10 Benchmarks

```
[ERROR]
Day not yet avaiable: 10
```

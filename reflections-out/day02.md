Day 2
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day02.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *2*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day01]: https://github.com/egnwd/advent/blob/2024/reflections-out/day01.md

*[Prompt][d02p]* / *[Code][d02g]*

[d02p]: https://adventofcode.com/2024/day/2
[d02g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day02.hs

Today showed me I need to warm up more to functional thinking.

For parsing, that was straight forward enough for a grid of numbers,
break into lines, then words, read the numbers and bubble the `Maybe` to the top with traverse.

```haskell
parse = traverse (traverse readMaybe . words) . lines
```

For part 1, I thought about it as running pairwise predicates and combining the results in accordance with the rules.

Specifically, it needed to be monotonically increasing or monotonically decreasing and the gaps cannot be too large.
That's 3 pairwise predicates and some simple boolean operations.

First I'll define a function for a pairwise rule:

```haskell
rule p x = and $ zipWith p x (tail x)
```

Next, we define our 3 predicates

```haskell
monotonicUp a b = a < b
monotonicDown a b = a > b
notTooLarge a b = inRange (1, 3) (abs $ a - b)
```

and finally we put them alltogether and count the number of reports that are safe:

```haskell
countTrue p = length . filter p

solveA x = countTrue $ (rule monotonicUp x || rule monotonicDown x) && rule notTooLarge x
```

I got a little stuck on the removals.
I thought I could simply check for a single violation of any rule at first.
Then I started thinking about looping over the list with indexes to drop certain items.
It was only when I thought about dropping items that I remembered the list functions `inits` and `tails`.

`inits` will create ever-expanding lists starting with `[]` and append the next element in the input list to the previous list in the output sequence, i.e. `inits [1,2,3] == [[], [1], [1,2], [1,2,3]]`.
Meanwhile, `tails` will do the opposite: starting with the input list it will remove the next item in the input list from the previous list in the output sequence, i.e. `tails [1,2,3] == [[1,2,3], [2,3], [3], []]`.

Therefore to get the list of possible reports that might be safe we concatenate the `inits` with some offset `tails`,
and check if the report passes all the rules.

```haskell
solveB x = countTrue $ any solveA $ zipWith (++) (inits x) (tail $ tails x)
```


*[Back to all reflections for 2024][reflections]*

## Day 2 Benchmarks

```
>> Day 02a
benchmarking...
time                 25.54 μs   (25.49 μs .. 25.58 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 25.43 μs   (25.39 μs .. 25.46 μs)
std dev              122.9 ns   (103.9 ns .. 152.1 ns)

* parsing and formatting times excluded

>> Day 02b
benchmarking...
time                 239.7 μs   (238.8 μs .. 241.2 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 238.7 μs   (238.4 μs .. 239.4 μs)
std dev              1.374 μs   (822.9 ns .. 2.570 μs)

* parsing and formatting times excluded
```

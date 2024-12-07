Day 7
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day07.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *7*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day01]: https://github.com/egnwd/advent/blob/2024/reflections-out/day01.md
[day02]: https://github.com/egnwd/advent/blob/2024/reflections-out/day02.md

*[Prompt][d07p]* / *[Code][d07g]*

[d07p]: https://adventofcode.com/2024/day/7
[d07g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day07.hs

Today we got to see lazy evaluation and the list moand at work.

We start by simply parsing each line as a `(Int, [Int])`:

```haskell
parser = parseLines ((,) <$> (pDecimal <* ": ") <*> many (pTok pDecimal))
```

Then for each line we can see if it's possible to create our test number.

To do so we can simply make all possible options, and see if there is the test number among them.

```haskell
canCreate test = elem test . go
    where
        go [] = mempty -- if the list is empty, there are no possibilities to create
        go [x] = return x -- if there is one item, return that as the only possibility
        go (a : b : xs) = do -- if there are at least two numbers
            op <- [(+), (*)] -- try each operator at each step
            go (a `op` b : xs) -- recurse with our new number and the rest of the list
```

The list monad takes care of pulling all hte branches into a single list!

For part 2 we can define our new concat operator `(||)` and add it to the list of possible operators:
```haskell
(||) :: Int -> Int -> Int
a || b = let e = succ . floor . logBase 10 . fromIntegral $ b
          in a * (10 ^ e) + b
```


*[Back to all reflections for 2024][reflections]*

## Day 7 Benchmarks

```
>> Day 07a
benchmarking...
time                 10.75 ms   (10.73 ms .. 10.77 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.76 ms   (10.75 ms .. 10.78 ms)
std dev              31.37 μs   (18.21 μs .. 52.78 μs)

* parsing and formatting times excluded

>> Day 07b
benchmarking...
time                 314.7 ms   (313.3 ms .. 316.2 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 315.1 ms   (314.7 ms .. 315.5 ms)
std dev              569.4 μs   (364.6 μs .. 859.0 μs)
variance introduced by outliers: 16% (moderately inflated)

* parsing and formatting times excluded
```

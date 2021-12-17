Day 8
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day08.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *8* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[13][day13]*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day01]: https://github.com/egnwd/advent/blob/2021/reflections-out/day01.md
[day02]: https://github.com/egnwd/advent/blob/2021/reflections-out/day02.md
[day03]: https://github.com/egnwd/advent/blob/2021/reflections-out/day03.md
[day04]: https://github.com/egnwd/advent/blob/2021/reflections-out/day04.md
[day05]: https://github.com/egnwd/advent/blob/2021/reflections-out/day05.md
[day06]: https://github.com/egnwd/advent/blob/2021/reflections-out/day06.md
[day07]: https://github.com/egnwd/advent/blob/2021/reflections-out/day07.md
[day09]: https://github.com/egnwd/advent/blob/2021/reflections-out/day09.md
[day10]: https://github.com/egnwd/advent/blob/2021/reflections-out/day10.md
[day11]: https://github.com/egnwd/advent/blob/2021/reflections-out/day11.md
[day13]: https://github.com/egnwd/advent/blob/2021/reflections-out/day13.md

*[Prompt][d08p]* / *[Code][d08g]*

[d08p]: https://adventofcode.com/2021/day/8
[d08g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day08.hs

Firstly, I'm going to parse each row into the following type:

```haskell
type Segments = Set Char
data Entry = Entry { signals :: [Segments], outputs :: [Segments] } deriving (Eq, Show)
```

I chose `Segments` to be a set of characters so that we can compare them ignoring what order they are in.
An entry is then a list of signals, and a list of outputs.

For example, `acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf`
would become

```haskell
Entry { signals = map fromList ["acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"]
      , outputs = map fromList ["cdfeb", "fcadb", "cdfeb", "cdbaf"]
      }
```

For part 1 we need to find out how many times 1, 4, 7, or 8 appear in the outputs.

We can do this quite simply using the fact that these four numbers have unique lengths.

```haskell
solve = length . filter (\o -> length o `elem` [2, 3, 4, 7]) . concatMap outputs
```

Part 2 is a bit trickier however.

Let's break down the task into:
 - Finding a **translation** for the **signals** of each row
 - Apply the **translation** to the **outputs** of each row

**Finding the translations:**

At a high level we can do the following:

```haskell
translations  = map (decodeEntry . signals) entries
decodeEntry e = find valid (choices e)
      where
          valid m = maybe False (all (`member` digits)) . traverse (translate m) $ e
```

So here we create a translation for each of the entries signals, and we do this by "decoding the entry".
`decodeEntry` finds a valid from a set of `choices` (we will define choices in a moment),
and a translation is valid if after using the transation on all the signals you are left with each being a digit.

_`digits` is a map of segments to numbers, e.g. `"cf" -> 1` is in the map_

So how do we determine the `choices`?

One option is to brute force all the possible maps from the characters a-g back to a-g.
However, this is slow and we can do better.

Firstly, we can use the unique numbers to massively restrict the list of translations.
Here is the full `choices` function that does just that, then we will break it down.

```haskell
choices :: [Segments] -> [Map Char Char]
choices = pickUnique . toList . fromListWith intersection . concatMap choices'
    where
        chooseFrom s n = toList . Data.Set.map (, n) $ s
        choices' s | size s == 2 = s `chooseFrom` one
                   | size s == 3 = s `chooseFrom` seven
                   | size s == 4 = s `chooseFrom` four
                   | size s == 7 = s `chooseFrom` eight
                   | otherwise = []
```

From the problem we know that any signal of length two will encode a `1`.
So from the example above we know that `"ab"` represents `1` (or `"cf"` as segments).
The issue is we don't know if `'a' -> 'c'` and `'b' -> 'f'`, or if `'a' -> 'f'` and `'b' -> 'c'`.

But we do have some possible options that we can represent as a mapping from `Char` to possible `Chars`.

For example, `"ab"` becomes: `[('a', ['c', 'f']), ('b', ['c', 'f'])]`.

We can then repeat this process for all the unique numbers, et voilà, we have the `choices'` function above!
_`one`, `seven`, ... are constants representing `"cf"`, `"acf"`, ... respectively._

After doing this for all the signals we can make a map from a character to a set of it's options.
To combine the options we've just created we can use set intersection.
As if one choice says that `'b' -> ['c', 'f', 'e']` and another says `'b' -> ['a', 'c', 'f']` we know that `'b' -> ['c', 'f']`.

Next we can turn this into a set of maps that can be created from these options where each input character maps to a unique output character.
Here I used the [`pickUnique` function from mstksg's common library from last year](https://github.com/mstksg/advent-of-code-2020/blob/master/src/AOC/Common.hs#L302).
It's a neat little function that keeps track of which characters are already used in the map and reduces each set of possible options to different maps of each possible pick.

Now that we have a valid map for each entry wew can apply the translation to the outputs.

**Applying the translations:**

```haskell
numberFromDigits :: (Foldable t) => t Int -> Int
numberFromDigits = foldl (\n d -> n * 10 + d) 0

solve :: [Entry] -> Int
solve ds = sum $ zipWith (\e t -> outputToNumber (outputs e) t) ds translations
    where
        outputToNumber o t = numberFromDigits $ map (pickNumber . translate t) o
        pickNumber n       = digits ! n
        translate t        = fromList . map (t !) . toList
        translations       = map (decodeEntry . signals) ds
        decodeEntry e = find valid (choices e)
            where
                valid m = maybe False (all (`member` digits)) . traverse (translate m) $ e
```

We zip each of the rows with the corresponding translation, and turn the translated outputs into a four digit number.

`outputToNumber` takes the outsputs and the translation and translates each output segments then looks up the output in the map of `digits` (`pickNumber`).
In the running example this will map `["cdfeb", "fcadb", "cdfeb", "cdbaf"]` to `["abdfg", "acdfg", "abdfg", "acdfg"]` then to `[5,3,5,3]`.
`numberFromDigits` digits then turns `[5,3,5,3]` into `5353`.

Finally, we sum the number we get from each row.


*[Back to all reflections for 2021][reflections]*

## Day 8 Benchmarks

```
>> Day 08a
benchmarking...
time                 5.454 ms   (5.400 ms .. 5.518 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 5.501 ms   (5.439 ms .. 5.623 ms)
std dev              270.0 μs   (184.1 μs .. 397.3 μs)
variance introduced by outliers: 26% (moderately inflated)

>> Day 08b
benchmarking...
time                 13.95 ms   (13.58 ms .. 14.56 ms)
                     0.993 R²   (0.984 R² .. 1.000 R²)
mean                 13.67 ms   (13.54 ms .. 14.00 ms)
std dev              477.7 μs   (169.0 μs .. 834.5 μs)
variance introduced by outliers: 11% (moderately inflated)
```

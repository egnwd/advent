Day 16
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day16.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[10][day10]* / *[11][day11]* / *[12][day12]* / *[13][day13]* / *[14][day14]* / *[15][day15]* / *16*

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

*[Prompt][d16p]* / *[Code][d16g]*

[d16p]: https://adventofcode.com/2021/day/16
[d16g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day16.hs

Today was a fun task for parsing.

Firstly we can define some types to help us represent a packet.

```haskell
data Packet
    = Literal Int Integer
    | Operator Int Operator [Packet]

data Operator
    = OpSum
    | OpProd
    | OpMin
    | OpMax
    | OpGT
    | OpLT
    | OpEQ
```

A packet can either be a literal or an operator.
A literal takes a version number and value.
An operator takes a version number, an operator and a list of sum packets.

The operator sum type is one of `sum`, `product`, `minimum`, `maximum`, `greater than`, `less than` and `equal`.

```haskell
parsePacket :: CharParser Packet
parsePacket = do
    v   <- toBinOrZero <$> takeP (Just "Version") 3
    typ <- toOp . toBinOrZero =<< takeP (Just "Operator") 3
    case typ of
      Nothing -> Literal v <$> parseLiteral
      Just op -> Operator v op <$> parseOperator

toBinOrZero :: (Integral a) => String -> a
toBinOrZero = fromMaybe 0 . preview binary

toOp :: Int -> CharParser (Maybe Operator)
toOp = \case
    0 -> pure $ Just OpSum
    1 -> pure $ Just OpProd
    2 -> pure $ Just OpMin
    3 -> pure $ Just OpMax
    4 -> pure   Nothing
    5 -> pure $ Just OpGT
    6 -> pure $ Just OpLT
    7 -> pure $ Just OpEQ
    n -> failure (Tokens <$> NE.nonEmpty (show n)) (S.singleton (Tokens . NE.fromList $ "Packet Type (0-7)"))
```

The top level parsing function takes the first 3 numbers and parses them as a binary number using `Numeric.Lens` which will read a binary number to a number.

Then the next 3 numbers are the operator type, which is either `Just` an operator or `Nothing` if it's a literal.
We then either parse a literal or operator into it's constructor with the version number.

To parse a literal we take chunks of 5 bits, consume the bits and either finish if the flag is `0` or parse another 5 if the flag is `1`.

```haskell
parseLiteral :: CharParser Integer
parseLiteral = toBinOrZero <$> parseLiteral'
    where
        parseLiteral' :: CharParser String
        parseLiteral' = do
            f <- anySingle
            d <- takeP (Just "Literal") 4
            rest <- if f == '0' then pure [] else parseLiteral'
            pure (d++rest)
```

To parse an operator we either parse an 11 bits and then parse just as many packets, or parse the next 15 bits as a length and parse the rest as a set of packets.

```haskell
parseOperator :: CharParser [Packet]
parseOperator = (char '0' *> parse15Operator) <|> (char '1' *> parse11Operator)
    where
        parse15Operator = do
            len <- takeP (Just "Length of subpackets") 15
            parseOrFail (many parsePacket) <$> takeP (Just "subpackets") (toBinOrZero len)
        parse11Operator = do
            cnt <- takeP (Just "Count of subpackets") 11
            count (toBinOrZero cnt) parsePacket
```

For the solving I used a [catamorphism](https://blog.sumtypeofway.com/posts/recursion-schemes-part-2.html).
This takes a recursive structure and folds it into a result. Some examples of _catamorphisms_ are: `length` or `sum`.
Catamorphisms are `foldr` generalised to any Functor.

For part 1 we need to count the version numbers:

```haskell
getVersionSum :: PacketF Int -> Int
getVersionSum (LiteralF v _) = v
getVersionSum (OperatorF v _ ps) = v + sum ps
```

`PacketF` is a autogenerated functor for the `Packet` type.
If we encounter a literal then we return the version number.
If we encounter an operator, we take it's version number and add the sum of all the sub version numbers.
Notice here there is no recursive call,
the _catamorphism_ has taken care of this and the `ps` variable will already be the version sum of each sub-packet.
_Notice that the structure is a [Rose Tree](https://en.wikipedia.org/wiki/Rose_tree)._

For part 2 we need to apply all the operations and calculate the packet, again we use a _catamorphism_.

```haskell
calculate :: PacketF Integer -> Integer
calculate (LiteralF _ l)     = l
calculate (OperatorF _ OpSum ps)   = sum ps
calculate (OperatorF _ OpProd ps)  = product ps
calculate (OperatorF _ OpMin ps)   = minimum ps
calculate (OperatorF _ OpMax ps)   = maximum ps
calculate (OperatorF _ OpGT [a,b]) | a > b  = 1
calculate (OperatorF _ OpLT [a,b]) | a < b  = 1
calculate (OperatorF _ OpEQ [a,b]) | a == b = 1
calculate _                        = 0
```

In this case, when we reach a literal, we simply return it's value.
For the recursive case we are given the `Integer` value of each sub packet,
so we just need to apply the operation.

_Note, for this problem we cound have mapped the operator types directly to the operator functions, e.g. `0 -> sum`.
However I decided to add hte intermediate type in case we wanted to do things like print the tree of packets etc._

And that's it! a super easy definition thanks to the recursive nature of hte rose tree.


*[Back to all reflections for 2021][reflections]*

## Day 16 Benchmarks

```
>> Day 16a
benchmarking...
time                 6.333 ms   (5.723 ms .. 7.255 ms)
                     0.923 R²   (0.887 R² .. 0.971 R²)
mean                 6.180 ms   (5.875 ms .. 6.494 ms)
std dev              1.023 ms   (807.2 μs .. 1.287 ms)
variance introduced by outliers: 80% (severely inflated)

>> Day 16b
benchmarking...
time                 4.776 ms   (4.175 ms .. 5.842 ms)
                     0.740 R²   (0.623 R² .. 0.865 R²)
mean                 5.463 ms   (4.979 ms .. 6.146 ms)
std dev              1.895 ms   (1.401 ms .. 2.578 ms)
variance introduced by outliers: 97% (severely inflated)
```

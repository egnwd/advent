Day 3
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day03.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *3* / *[4][day04]* / *[5][day05]* / *[6][day06]* / *[7][day07]* / *[8][day08]* / *[9][day09]* / *[13][day13]*

[reflections]: https://github.com/egnwd/advent/blob/main/reflections.md
[day01]: https://github.com/egnwd/advent/blob/2021/reflections-out/day01.md
[day02]: https://github.com/egnwd/advent/blob/2021/reflections-out/day02.md
[day04]: https://github.com/egnwd/advent/blob/2021/reflections-out/day04.md
[day05]: https://github.com/egnwd/advent/blob/2021/reflections-out/day05.md
[day06]: https://github.com/egnwd/advent/blob/2021/reflections-out/day06.md
[day07]: https://github.com/egnwd/advent/blob/2021/reflections-out/day07.md
[day08]: https://github.com/egnwd/advent/blob/2021/reflections-out/day08.md
[day09]: https://github.com/egnwd/advent/blob/2021/reflections-out/day09.md
[day13]: https://github.com/egnwd/advent/blob/2021/reflections-out/day13.md

*[Prompt][d03p]* / *[Code][d03g]*

[d03p]: https://adventofcode.com/2021/day/3
[d03g]: https://github.com/egnwd/advent/blob/main/src/AOC/Challenge/Day03.hs

I have not really liked any of the iterations of Day 3, but I learnt something new for this one, so I'm happy with that.

I learnt about [apomorphisms](https://ipfs.io/ipfs/QmTppu1VDAQWsdiyVSZX6qb8PErdpwzNP2oKfEhcgaBvWR/guide-to-morphisms.pdf)!

Before we get there though, let's look at part 1.

We have many rows of binary numbers, so we need to parse the strings into a many lists of bits.
I'm going to use the following data types:

```haskell
type Bit = Finite 2
type Bits = [Bit]

zero, one :: Bit
zero = finite 0
one  = finite 1
```

`Finite 2` represents a number type that has two inhabitants, `0` and `1`.

So in order to parse we can simply do

```haskell
parser :: String -> [Bits]
parser = map (map (finite . fromIntegral . digitToInt)) . lines
```

For part 1 we need to collect a binary number for the most common bits in each column,
and a binary number for the least common bits. We can do this by transposing the matrix of bits so that we are iterating over the columns and getting the most and least significant bit.

```haskell
-- | Count the number of 1s and 0s, then compare
mostCommonBit :: Bits -> Bit
mostCommonBit bs = if ones > zeros then one else zero
    where
        (zeros, ones) = foldr go (0,0) bs
        go :: Bit -> (Int, Int) -> (Int, Int)
        go 0 = first succ
        go 1 = second succ
        go _ = id

-- | Note the least common bit will just be the most common bit flipped
mostAndLeastCommonBit :: Bits -> (Bit, Bit)
mostAndLeastCommonBit bs = let b = mostCommonBit bs in (b, 1-b)

getMostAndLeastCommonBitsPerColumn :: [Bits] -> [(Bit, Bit)]
getMostAndLeastCommonBitsPerColumn = map mostAndLeastCommonBit . transpose
```

Now that we have these values we need to turn the binary numbers to decimals and multiply them together.

```haskell
solve bs = let (gamma, epsilon) = unzip (getMostAndLeastCommonBitsPerColumn bs)
            in gamma * epsilon
```

_If you aren't familiar, `unzip` takes a list of pairs (`[(a,b)]`) and creates a pair of lists `([a], [b])`._

Part 1 done!

Part 2 is where we get into _apomorphisms_.

Firstly, I found [this blog post](https://blog.sumtypeofway.com/posts/recursion-schemes-part-3.html) useful to give an insight into how _apomorphisms_ are constructed.

To very poorly summarise: an _apomorphism_ is the dual of a _paramorphism_, which in turn is a _catamorphism_ but with additional context of the structure that you are recursing on.
A _catamorphism_ is a way to collapse a structure down to some value, and is the most familar of the _-morphisms_,
for example `foldr`, `sum` & `length` are all catamorphisms (I'm pretty sure).
So a _paramorphism_ is a way of collasing a structure down to a value, but at each stage you have some additional context of the unmodified structure. If it helps the types (I'm using the list functor here for ease) look like this:

```haskell
cata :: ([a] ->       b)  -> [a] -> b
para :: ([a] -> ([a], b)) -> [a] -> b
```

So, an _apomorphism_ is the dual of the _paramorphism_, and the way I have tried to think of it (which may be wrong) is:
you take a seed value and build a structure by providing either the rest of the structure, or a bit of the structure and the next seed. The type is roughly:

```haskell
apo :: (b -> Either [a] (a, b)) -> b -> [a]
```

So... how do we use this...

Going back to the problem we need to calculate the oxygen rating and carbon dioxide rating and multiply them together.
To get the oxygen rating we need to find the number matches the **most common bit** of each column by filtering out numbers that don't match until we are left with one.

_From here I will use the non specific type of `apo`._

Here is the base functor for a list.
I'm not going to go into what that is, but the structure looks a lot like a cons list and
it basically has some part of the list structure in the `Cons a` then keeps the rest of the structure in `b`.
For much better information I would recommend [this blog post](https://blog.sumtypeofway.com/posts/recursion-schemes-part-4-point-5.html).

```haskell
data ListF a b = Nil | Cons a b
```

Now we can think about our initial seed: the rows of binary numbers.
And what we can to produce: a list of bits that represent the oxygen rating (for now).

```haskell
oxygen :: [Bits] -> ListF Bit (Either Bits [Bits])
oxygen bs = ...
```

This looks sort of like the type definition from earlier for `apo`, the main difference is the `(a, b)` is now a `ListF` wrapping the `Either`.

The next task for the oxygen rating is to work out the **most common bit** for the column.

```haskell
mcb = (mostCommonBit . map head) bs
```

We can do this by reusing our `mostCommonBit` function from part 1 and running it on the head of each binary number in our seed.

Next, we need to get all the numbers in the seed that start with this `mcb`.

```haskell
ns = filter ((== mcb) . head) bs
```

Again fairly simple, just filter the list where the head is equal to the **most common bit**.

The final part is the interesting part:

```haskell
go = case ns of
    []  -> Nil -- There are no numbers (uh no, the problem might be broken)
    -- If there is only one number left,
    -- return part of the structure: the current mcb cons'd to all the remain bits of the final number
    [n] -> Cons mcb (Left (tail n))
    -- For anything else,
    -- return part of the structure: the current mcb cons'd to the next seed
    -- (all the binary numbers after removing the current column)
    _   -> Cons mcb (Right (map tail ns))
```

Visually it looks a little like doing this:

```
Current seed:

0 | 0 1 0 0
1 | 1 1 1 0
1 | 0 1 1 0
1 | 0 1 1 1

Filtered:

1 | 1 1 1 0
1 | 0 1 1 0
1 | 0 1 1 1

Next Seed:
Cons 1 (
    1 | 1 1 0
    0 | 1 1 0
    0 | 1 1 1)
```

The carbon dioxide follows the same pattern, we just use **least common bit** not **most common bit**.
Finally, we get teh values and multiply them together:

```haskell
solve bs = o * c
    where
        o = binToDec $ apo oxygen bs
        c = binToDec $ apo carbonDioxide bs
```

Phew! and Done!


*[Back to all reflections for 2021][reflections]*

## Day 3 Benchmarks

```
>> Day 03a
benchmarking...
time                 2.616 ms   (2.580 ms .. 2.653 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 2.579 ms   (2.545 ms .. 2.617 ms)
std dev              127.0 μs   (94.73 μs .. 169.5 μs)
variance introduced by outliers: 31% (moderately inflated)

* parsing and formatting times excluded

>> Day 03b
benchmarking...
time                 509.8 μs   (508.6 μs .. 511.3 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 508.9 μs   (508.1 μs .. 510.1 μs)
std dev              3.314 μs   (2.671 μs .. 4.362 μs)

* parsing and formatting times excluded
```

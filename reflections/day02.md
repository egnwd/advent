Moving around a 2D map, another AoC classic challenge.

My initial solution used recursion and word splitting for the solution,
but I have an affinity for parsing and the state monad, so that's what I'll describe here.

To parse we can read many tuples of `(Direction, Distance)` and have that as our instruction set.
The parser combinators make this quite nice to do:

```haskell
parser = do
  dir <- pTok $ (Forward <$ "forward") <|> (Up <$ "up") <|> (Down <$ "down")
  dist <- pDecimal
  return (dir, dist)
```

Then to solve part 1 we just need to move through the instructions, summing the scaled vectors together.
I used the `V2` type from `Linear.V2` to represent points as they are 2D vectors with the added bonus of
being able to easily to component-wise addition of vectors.


To get the vectors:

```haskell
positionUpdate = \case
    Forward -> V2 1 0
    Up      -> V2 0 (-1)
    Down    -> V2 0 1
```

```haskell
solve = product . sum . map (\(d,n) -> pure n * positionUpdate d)
```

After the submarine's epic journey, we get 1 star!

For part two, I used the State monad.
You can quite easily use a fold, which I did initially but it was quite messy, I felt this was easier to read.

To update the state we have the following, for each instruction, update the location or aim using:
```haskell
moveSubmarine Forward n = do
        a <- use aim
        loc += V2 n (n*a)
moveSubmarine d n = aim += n * aimUpdate d
    where
        aimUpdate = \case
            Up   -> -1
            Down -> 1
            _    -> 0
```

then we just extract the final `loc` and get the `product` like last time.

Submarine successfully piloted!

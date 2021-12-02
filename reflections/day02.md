Moving around a 2D map, another AoC classic challenge.

My initial solution used recursion and word splitting for the solution,
the second used State, and that too was more difficult to understand, and felt more imperative.

The version here uses Monoids to combine the values (part a ended up similar to my first fold attempt).

To parse we can read many `Sum`mable `Points` and have that as our instruction set.
The parser combinators make this quite nice to do:

```haskell
parser = do
  dir <- pTok $
        (Sum . flip V2 0     <$ "forward")
    <|> (Sum . V2 0 . negate <$ "up")
    <|> (Sum . V2 0          <$ "down")
  dir <$> pDecimal
```

Then to solve part 1 we just need to combine the instructions.
I used the `V2` type from `Linear.V2` to represent points as they are 2D vectors with the added bonus of
being able to easily to component-wise addition of vectors.

```haskell
solve = product . getSum . mconcat
```

After the submarine's epic journey, we get 1 star!

For part two, I wrote a new type `Submarine` with a location and aim.

```haskell
data Submarine = Sub { loc :: Point, aim :: Int }
```

this time to combine we must implement `Monoid`, and first Semigroup.

```haskell
instance Semigroup Submarine where
    (Sub v a) <> (Sub (V2 x' y') a') = Sub (v + V2 x' (y' + x' * a)) (a + a')

instance Monoid Submarine where
    mempty = Sub (pure 0) 0
    mconcat = foldl (<>) mempty
```

`(<>)` combines two commands, adding the forward components `x+x'`,
updates the depth taking into account the previous aim and the forward distance
`y + y' + x' * a` (for the `up` & `down` commands `x'` & `y'` are `0` so that term cancels),
and finally the aim is updated (for `forward` this is `0` so is also a noop).

Then to combine we do similar to part 1:
```haskell
solve = product . loc . mconcat
```

where `loc` gets the vector from the `Submarine` akin to `getSum` getting the vector from `Sum`.

Submarine successfully piloted!

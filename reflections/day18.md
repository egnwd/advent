I really enjoyed day 18, despite the difficulty ramping up.
We got to define some nice infix operators too!

First let's define some types to represent our snail numbers and operations that we need to perform.

```haskell
data SnailNum = SN Int | SP SnailNum SnailNum deriving (Eq)
data Todo = NoWork | PlaceLeft Int | PlaceRight Int
```

A snail number is either a number, or a pair of snail numbers.
_A.k.a. a binary tree with only values at the leaves._

`Todo` represents work that will need to be done on the snail numbers whilst exploding them.

Let's start by defining the `explode` and `split` functions that are used when reducing the snail numbers.

```haskell
explode, split :: SnailNum -> Maybe SnailNum
```

Both take a `SnailNum` and maybe return a `SnailNum` if the explode, or split, are applicable.

Split has a simpler definition so we will start with that.

```haskell
split :: SnailNum -> Maybe SnailNum
split = histo go
    where
        go :: SnailNumF (Cofree SnailNumF (Maybe SnailNum)) -> Maybe SnailNum
        go (SNF n) = do
            guard $ n >= 10
            let n' = (fromIntegral n / 2) :: Double
            pure $ SN (floor n') `SP` SN (ceiling n')
        go (SPF l r) = asum [(`SP` out r) <$> extract l, SP (out l) <$> extract r]
```

That's the full definition, but before we approach what it means we need to understand [_histomorphisms_](https://blog.sumtypeofway.com/posts/recursion-schemes-part-4.html).
A histomorphism is similar to a _catamorphism_ or _paramorphism_, but it gives us access to the full _history_ of the computations.
It does this through the use of `Cofree`, which is defined by:

```haskell
data Cofree f a = a :< f (Cofree f a)
```

but what does this mean, well we have a recursive type that is a pair of an `a` and a `f (Cofree f a)`.
`a` acts as an _annotation_ for the current level, and the `f` is a functor of `Cofree f a`.

As part of the `Comonad` (A `Comonad` is a `Cofree Comonad` for `f` if [everfsdf d as asehfb hdfsdfjab asdfhb](https://hackage.haskell.org/package/free-5.1.7/docs/Control-Comonad-Cofree.html)*) definition we also get the function `extract`.

_* I prefer the more intuitive:_

> cofree comonads are quite useful for annotating syntax trees, or talking about streams

Anyway, back to `extract`, it allows us to _extract_ the `a` from a Comonad `w a`.
_Fun fact: we use `w` for Comonad as it's an upside down `m` that is used for Monad._

Let's looks at the base case:
A snail number splits if it is a number value and it's greater than or equal to 10.
It becomes a pair of the number split in half (with the second number taking any extra).

```haskell
go (SNF n) = do
    guard $ n >= 10
    let n' = (fromIntegral n / 2) :: Double
    pure $ SN (floor n') `SP` SN (ceiling n')
```

The guard takes care of the `>= 10` condition, if it fails we return `Nothing` (indicating no split occured).
Otherwise we return a pair with the number split in 2, the first rounded down, the second rounded up.

The recursive case is where we look at the `Cofree`:

```haskell
go (SPF l r) = asum [(`SP` out r) <$> extract l, SP (out l) <$> extract r]
```

`l` and `r` are both Cofree, using `SnailNumF` as the recursive functor, and a maybe snail number as the annotation.
We use `asum` to choose the first branch that split a snail number, if any.
`extract` takes out the annotation, i.e. the maybe split snail number, and `out` takes the recursive structure and and projects it to the snail number.

```haskell
out :: Cofree SnailNumF a -> SnailNum
out (_ :< SNF n) = SN n
out (_ :< SPF l r) = SP (out l) (out r)
```

_There might be a better way of doing this._

Next we can do `explode` in a similar manner:

```haskell
explode :: SnailNum -> Maybe SnailNum
explode inN = snd <$> histo go inN 0
    where
        go :: SnailNumF (Cofree SnailNumF (Int -> Maybe (Todo, SnailNum)))
           -> Int -> Maybe (Todo, SnailNum)
        go (SNF _) _ = Nothing
        go (SPF (nl :@: nr) r) 3 = Just (PlaceLeft nl, SP (SN 0) (nr ^@+ out r))
        go (SPF l (nl :@: nr)) 3 = Just (PlaceRight nr, SP (nl @+^ out l) (SN 0))
        go (SPF l r) d = asum $ zipWith (\f sn -> f <$> extract sn (d+1)) [explodeLeft, explodeRight] [l,r]
              where
                explodeLeft (PlaceRight nr, sn) = (NoWork, SP sn (nr ^@+ out r))
                explodeLeft (todo, sn) = (todo, SP sn (out r))

                explodeRight (PlaceLeft nl, sn) = (NoWork, SP (nl @+^ out l) sn)
                explodeRight (todo, sn) = (todo, SP (out l) sn)
```

However this time the annotation is a function that takes a depth and returns a `Maybe (Todo, SnailNum)`.

The base cases are as follows:

```haskell
go (SNF _) _ = Nothing
```

A normal number has no work to do and doesn't need exploding, so we return nothing.

```haskell
go (SPF (nl :@: nr) r) 3 = Just (PlaceLeft nl, SP (SN 0) (nr ^@+ out r))
```

If at depth 3 (starting from 0) we see a pair of ordinary numbers at the next layer,
we have work to do, we need to tell the layer above us to add the left number in it's rightmost leaf (`PlaceLeft nl`)
and the number becomes a pair of `[0,nl ^@+ out r]`. where `^@+` adding a number to the leftmost leaf of the right snail number.
i.e. `[[nl,nr],r]` becomes `Just (PlaceLeft nl, [0, nr+r])` (assuming `r` is just a number).

```haskell
go (SPF l (nl :@: nr)) 3 = Just (PlaceRight nr, SP (nl @+^ out l) (SN 0))
```

This case is the same before the but operating on the right branch first.
_N.B. we must be sure to match on the left branch first as to obey the left to right ordering of the explode operation._

Finally, the recursive case:

```haskell
go (SPF l r) d = asum [explodeLeft <$> extract l (d+1), explodeRight <$> extract r (d+1)]
      where
        explodeLeft (PlaceRight nr, sn) = (NoWork, SP sn (nr ^@+ out r))
        explodeLeft (todo, sn) = (todo, SP sn (out r))

        explodeRight (PlaceLeft nl, sn) = (NoWork, SP (nl @+^ out l) sn)
        explodeRight (todo, sn) = (todo, SP (out l) sn)
```

similar to before, we find the first branch, if any that explodes a number.
We do this by getting the annotation and passing down the next depth.

If we successfully explode the left branch we apply `explodeLeft` which looks for an `PlaceRight` command and will perform the insertion,
returning the fact there is no more work to do.

Similar logic applies if we match on the right branch.

From here we just need to define snail addition `(@+)`:

```haskell
(@+) :: SnailNum -> SnailNum -> SnailNum
a @+ b = applySnailRules (SP a b)
    where
        applySnailRules n = maybe n applySnailRules (asum [explode n, split n])
```

We make a pair of the numbers and repeatedly apply the explode and split rules until neither apply, returning the final reduced number.

The part 1 solution is to find the number after adding all the numbers, and get it's magnitude.

The magnitude can be defined by defining a _catamorphism_ over the snail number.

```haskell
magnitude :: SnailNum -> Int
magnitude = cata go
    where
        go (SNF n) = n
        go (SPF l r) = (3 * l) + (2 * r)
```

```haskell
solvea sn = magnitude (foldl1 (@+) sn)
```

Part 2 then uses a lot of what we defined for part 1, but looks for the maximum magnitude of any pair:

```haskell
solveb :: [SnailNum] -> Int
solveb ns = getMax . foldMap (Max . magnitude) $ do
    (x:ys) <- tails ns
    y <- ys
    [x @+ y, y @+ x]
```

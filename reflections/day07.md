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

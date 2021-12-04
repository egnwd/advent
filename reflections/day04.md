Bingo against a giant squid is a funny idea 😆

Today we got to learn about two Semigroups! `First` and `Last`,
they are fairly self explanatory, `First` will give you the left (first) argument to `(<>)`
and `Last` will give you the second (last) argument to `(<>)`.

_N.B. `Last` is the `Dual` of `First`._

To parse we need two things, the list of numbers and all the cards.

```haskell
data BingoNumber = Unmarked Int | Marked Int deriving (Eq, Show)
type BingoCard = [[BingoNumber]]

parse :: String -> ([Int], [BingoCard])
```

Here we can represent a number as either marked or unmarked (note this is just `Either` with a nicer name).

Then we can define our solve function to take the list of numbers, the boards, and return the score, maybe.

```haskell
solve :: [Int] -> [BingoCard] -> Maybe Score
```

(We will look at `Score` later.)

Then as we see a number, `n`, we mark all our cards.

```haskell
newCards n cards = cards'
  where
    cards' = cards <&> mapped.mapped %~ mark
    mark (Unmarked x) | x == n = Marked x
    mark x = x
```

We can define what it means for a card to have won:

```haskell
winner :: BingoCard -> Bool
winner cards = row cards || column cards
    where
        column = row . transpose
        row = any (all isMarked)

isMarked :: BingoNumber -> Bool
isMarked = \case
    Marked _ -> True
    _ -> False
```

Next, we work out the scores for all the winners this round.

```haskell
scoreWinners cards' n = winners
  where
      winners = foldMap (Just . First . scoreCard) w
      w = filter winner cards'
      scoreCard card = (sum . unmarked $ card) * n
      unmarked = map unmarkedValue . concat

unmarkedValue :: BingoNumber -> Int
unmarkedValue (Unmarked n) = n
unmarkedValue (Marked _)   = 0
```

and here is where we get to see `First`.

We've taken our cards and found the winners, then we score them using the score system from the problem statement,
and next we make a `Maybe (First Int)` with `Just . First`. This represents (the maybe is to represent the fact we have a score).

`foldMap` will the return `Nothing` if there are no winners this round, or `Just (First x)` where `x` is the score of the first winner.

We then recurse on the rest of the numbers and the boards that haven't won yet.

```haskell
solve (n:ns) cards = scoreWinners cards' n <> solve ns (filter (not . winner) cards')
  where
    cards' = newCards n cards
```

Now for part two all we need to do is swap `First` for `Last`!!

Now, hopefully the squid doesn't eat us.

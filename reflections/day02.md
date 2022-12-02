Hmm... Well I messed up.

I had the solution fairly quickly by hard coding the mappings suggested and lots of lookups...
and then decided the intended solution was to find the *maximum* score given any strategy...

which does have a nice little form

```haskell
strategies = map (zip "ABC") (permutations "XYZ")
```

and then you simply need to find the maximum from applying all the strategies.


But that's wrong, so let's simplify.

First we can set up a parser, using the default lookup:

```haskell
parseGame = do
    opp <- Rock <$ "A" <|> Paper <$ "B" <|> Scissors <$ "C"
    pSpace
    you <- Rock <$ "X" <|> Paper <$ "Y" <|> Scissors <$ "Z"
    return (opp, you)
```

and then we can score a game given some rules on the winner:

```haskell
Paper    `play` Rock     = Win
Rock     `play` Scissors = Win
Scissors `play` Paper    = Win
a        `play` b
  | a == b = Draw
  | otherwise = Lose
```

I wasn't hugely happy with this, I tried an implementation with `Ord` and another with `Map`
but this was still the simplest, and it's a total function as we've parsed the inputs into nice types.

Then given the scoring functions set out in the puzzle:

```haskell
score you out = scoreRPS you + scoreOutcome out

scoreRPS = \case
    Rock     -> 1
    Paper    -> 2
    Scissors -> 3

scoreOutcome = \case
    Win  -> 6
    Draw -> 3
    Lose -> 0
```

we can score a game simply using ``\opp you -> score you (you `play` opp)``.

Finally.

Then for part two, much the same we just need a different lookup and different parser.

```haskell
parseStrategy = do
    opp <- Rock <$ "A" <|> Paper <$ "B" <|> Scissors <$ "C"
    pSpace
    out <- Lose <$ "X" <|> Draw <$ "Y" <|> Win <$ "Z"
    return (opp, out)

whatToPlay a        Draw = a
whatToPlay Paper    Win  = Scissors
whatToPlay Rock     Win  = Paper
whatToPlay Scissors Win  = Rock
whatToPlay Paper    Lose = Rock
whatToPlay Rock     Lose = Scissors
whatToPlay Scissors Lose = Paper
```

again, I'm not a fan of huge lookups but there we go.

putting together what we already have gives us ``\opp out -> score (whatToPlay opp out) out``.

Two stars, but not the most satisfying.

I feel like there is a better solution for today that avoids checking all points, but for now here is the naïve version:

For part 1 we need a way of scoring a particular horizontal position.
The score here is the sum of the absolute differences from the crabs to the position in question.

```haskell
score :: [Int] -> Int -> Int
score crabs position = sum . map (abs . subtract position) $ crabs
```

The we need to check all the positions from the minimum crab to the maximum crab.

```haskell
solve [Int] -> Int
solve crabs = minimum . map (score crabs) $ [minimum cs .. maximum cs]
```

And there we go!

For part 2, then need to confess my original implementation of part 1 was:

```haskell
solve [Int] -> Int
solve crabs = minimum . map (score crabs) $ crabs
```

as I assumed we had to pick a crab for them to congregate on,
and one of my crabs was on the right spot.

For part 2 I then just needed to tweak the score function to be

```haskell
triangularNumber :: Int -> Int
triangularNumber x = x * (x + 1) `div` 2

score :: [Int] -> Int -> Int
score crabs position = sum . map (triangularNumber . abs . subtract position) $ crabs
```

_...and then you debug why you can't get the right answer and think you can no longer remember what a triangular number is for 20 minutes,
despite watching absurds amounts of [sudoku videos](https://www.youtube.com/c/CrackingTheCryptic)..._

...until you realise all the crabs could congregate on any of the spots.

_sigh._

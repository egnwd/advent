Today was interesting, and our first time this year seeing a non-Int answer.

For our first task we need to parse the data.

I chose to represent the paper as a set of points (`Set (V2 Int)`) and the folds as a custom data type (`Fold`).

```haskell
type TransparentPaper = Set (V2 Int)
data Fold = FX Int | FY Int
```

_`V2` is from the `Linear` library and represents a vector of 2 items._

For details on the full parsing check out the code but the main idea is:
 - For a point split on the ',' and make a `V2` from the two numbers
 - Turn all the points into a set
 - For a fold, if it's `"x="` then parse the number into the `FX` constructor, else parse it into the `FY` constructor

and that's pretty much it!

So for part 1 we just need to apply the first fold and count the remaining points,
nicely alluding to what part 2 is going to be and making sure we are on the right track.

To start with I needed a function that decides if a point is beyond the fold or not.
From the problem we know that points that get moved are "greater" than the fold line.

So we can define a `beyondFold` function like so:

```haskell
beyondFold :: V2 Int -> Fold -> Bool
beyondFold (V2 x y) = \case
    FX fx -> x > fx
    FY fy -> y > fy
```

next we need to remap any points that lie beyond the fold to new positions on the folded side.

```
┌─────┐
│ #   │
│ ▲   │
├─┼───┤
│ │   │
│ #   │
└─────┘
```

in this example the **#** is being mapped to above the line `y=2`.

This results in a transformation from `(1,4)` to `(1,0)`.

More generally we can get this by, for a Y-fold, keeping `x` the same,
and then subtracting from the line (`2`) the distance from the point to the line (`4-2`).
`fy - (y - fy)` which simplifies to `2 * fy - y`.
Similar logic can be used for the X-fold, replacing `y` & `fy` for `x` & `fx`, respectively.

This gives us our remapPoint function:

```haskell
remapPoint :: Point -> Fold -> Point
remapPoint (V2 x y) = \case
    FX fx -> V2 (2*fx-x) y
    FY fy -> V2 x        (2*fy-y)
```

The only remaining thing to do is partition the points, remap the ones beyond the fold and union the remapped points with the ones that didn't move.

```haskell
applyFold :: TransparentPaper -> Fold -> TransparentPaper
applyFold points f = union remappedPoints remainingPoints
    where
        remappedPoints = map (`remapPoint` f) pointsToMove
        (pointsToMove, remainingPoints) = partition (`beyondFold` f) points
```

_You could also do this with a single `map` with an `if`, but I preferred this approach._

Finally, we just need to count the number of points after the first fold:

```haskell
solve paper (f:fs) = size $ applyFold paper f
```

Woo, part 1 done!

For part 2 we need to apply all the folds and read the string from the ASCII art.

Applying all the folds is simple, we use:

```haskell
solve paper folds = foldl applyFold paper folds
```

This results in a set of points that we can print, squint and type into the answer box.

_However, that doesn't get us the final answer..._

To do this I used the [`Advent.OCR` library](https://github.com/mstksg/advent-of-code-ocr),
which exposes a `parseLettersWith` function which takes a function for extracting the x coordinate, one for the y coordinate and a set of points.

```haskell
readCode paper = parseLettersWith (view _x) (view _y) paper
```

Now we are ready to do some thermal imaging!

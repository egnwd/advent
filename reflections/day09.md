Part 1 today was simple enough.

Firstly we can parse in the numbers as a `Map (V2 Int) Int`,
and as a cheat for later we can ignore all the spots with `9`, you can imagine them as "super high walls" in our map.

```haskell
type Landscape = Map (V2 Int) Int

parse :: String -> Landscape
parse = parseAsciiMap (mfilter (<9) . pure . digitToInt)
```

where `parseAsciiMap` is a helper method that zips the indexes of a grid with the value,
and a method for generating the value, `Nothing` removes it from the map and `Just x` places `x` into the map.

To find the low points we can filter the map to entries that have values lower than all the neighbours.
To do that we get all the neighbours by adding "North", "East", "South" and "West" to the index in the map (the key).
_N.B. this might result in a point off the map!_

Then we ensure that the current value is less than all these points (if they exist!)
Finally, we can sum all the scores of the low points.

```haskell
score :: Int -> Sum Int
score n = Sum (1+n)

neighbours k = (k +) <$> [V2 0 (-1), V2 1 0, V2 0 1, V2 (-1) 0]

solvea :: Landscape -> Int
solvea = getSum . foldMapWithKey (const score) . findLowPoints
  where
    findLowPoints nss = M.filterWithKey isLowest nss
    isLowest k h = all (maybe True (h <) . (`M.lookup` nss)) (neighbours k)
```

Now for part 2!

For part 2, I used another _apomorphism_! We are tasked with find the product of the sizes of the 3 largest basins.
Our first task is to find the sizes of the basins. All basins have a sink which is the lowest point, so we can map over these and build the basin from this _seed point_.

First let's generate the seeds, we simply find the low points as before, get the locations and turn it into a tuple of `(seen, pointsToCompute)`:

```haskell
lowPointSeeds = map (\l -> (S.empty, S.singleton l)) . M.keys . findLowPoints $ land
```

now we build our basins from a given seed.

```haskell
buildBasin
    :: M.Map Point (S.Set Point)                           -- | Map from point to neighbouring values
    -> Landscape                                           -- | Original grid
    -> (S.Set Point, S.Set Point)                          -- | Seen points, and next points
    -> ListF Int (Either [Int] (S.Set Point, S.Set Point)) -- | ListF of current region sizes over the next regions to visit
buildBasin ns land (seen, next) = Cons (S.size next) go
    where
        neighbouringBasinLocations = M.keysSet . M.restrictKeys land . S.unions . S.map (fromMaybe S.empty . (`M.lookup` ns))
        next' = neighbouringBasinLocations next `S.difference` seen
        seen' = S.union seen next
        go = if null next'
                then Left []
                else Right (seen', next')
```

Here we find all the neighbouring basin locations by looking up the neighbours for our each of our next points to look at,
We then union all these neighbouring points, and intersect the points with our land (to remove any off the edge).

We then remove from this set all the points we've seen before.

If there is nothing in this set we have found the full region, so we finish by returning `Left []`,
otherwise we return `Right (seen', next')` where `seen'` is the union of what we've seen with the points we have just computed.

To build the `ListF` functor we have `Cons (S.size next)` which adds the size of the sub-basin we have just computed to the list.

Then to get the sum we run `sum . apo (buildBasin allNeighbours land) $ lowPointSeeds` where

```haskell
allNeighbours = M.mapWithKey (const . S.fromList . neighbours) land
```

now that we have all the region sizes we find the largest 3 and get the product.

```haskell
solveb :: Landscape -> Int
solveb land = product . largest 3 . map getBasinSize $ lowPointSeeds
    where
        largest n = take n . sortOn negate
        getBasinSize = sum . apo (buildBasin allNeighbours land)
        allNeighbours = M.mapWithKey (const . S.fromList . neighbours) land
        lowPointSeeds = map (\l -> (S.empty, S.singleton l)) . M.keys . findLowPoints $ land
```

And there we are!

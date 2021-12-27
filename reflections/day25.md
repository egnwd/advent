Christmas Day!

Another iteration based approach, just needing the index of the fixed point.

Let's start by defining a stepping function for the sea cucumbers.

```haskell
step :: Point -> Map Point Dir -> Map Point Dir
step bounds = stepDir South . stepDir East
    where
        stepDir :: Dir -> Map Point Dir -> Map Point Dir
        stepDir dir mp = mapKeys moveCuke mp
            where
                dirs = keysSet . filter (==dir) $ mp
                moveCuke p = let p' = mod <$> p + dirVec dir <*> bounds
                                 isDir = member p dirs
                              in if isDir && p' `M.notMember` mp then p' else p
```

We define the landscape of sea cucumbers as a map from it's location to which direction it's facing.
To step the cucumbers we first move all the East pointing cucumbers, then the south facing ones.
And so to step the cucumbers in a direction we map all the locations to a new location,
if the cucumber isn't facing in the direction of interest we don't move it, if it is in the direction of interest,
and there is no cucumber in the new location, we set it's location to the new location.
We calculate the new location by adding the direction vector to the original point and taking that modulo the bounds of the map.
_N.B. East -> `V2 1 0` and South -> `V2 0 1`_

We then just need to find the fixed point:

```haskell
solve mp = go 1 mp
  where
    go idx !x
        | x == y    = idx
        | otherwise = go (idx+1) y
      where
        y = step (findMax mp) x
```

Christmas saved! Merry Christmas!!

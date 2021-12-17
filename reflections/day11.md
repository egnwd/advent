I again enjoyed the theme on this day, I liked the idea of the flashing octopuses.

Part 1 and 2 had a nice similar foundation and just had different stopping criteria.

I'll first explain my core logic, then cover the stopping criteria.

We need a function that can take a step for all the octopuses, then we iterate this step function.
Similar to how you might solve a conways game of life problem.

To define the step function we want something that takes the octopuses and gives us a new state of octopuses:

```haskell
type Octopuses = M.Map Point Int

step :: Octopuses -> Octopuses
step os = undefined
```

The algorithm we need steps the energy level of each octopus once,
then again for each octopus next to an octopus that flashed (note that if an
octopus is adjacent to 2 octopuses that flash, it flashes twice).

It does this until it settles (octopuses can flash only once per step)
and then the flashed octopuses reset their energy.

I broke this into two stages:

1. Finding the settled state (fixed point)
1. Resetting the grid

A fixed point takes a function from some state to another state of the same time,
and keeps applying this function (starting with some initial state) until it sees the same state twice,
and returns that state.

My fixed point function takes a frequency map of octopuses to give an energy boost, and how many times to do so, and the map of octopuses.

```haskell
next :: (M.Map Point Int, Octopuses) -> (M.Map Point Int, Octopuses)
next (keys, os) = (keys', fst <$> os')
    where
        keys' = (`M.restrictKeys` M.keysSet didn'tFlash) . freqs . concatMap allNeighbours . M.keys $ flashed
        (flashed, didn'tFlash) = M.partition snd os'
        os' = M.mapWithKey (boost keys) os
```

So we first map the octopuses `os` to a new set of octopuses `os'`.
We partition this into those that flashed and those that did not.
We then find the neighbours of the octopuses that flashed,
and turn that into a frequency map of how many times a neighbour octopus appears.

We then restrict that to octopuses that didn't flash, to ensure nothing flashes twice.

Then we return the octopuses to boost next time and the new octopuses.
If there are no more octopuses to flash the octopuses are unchanged and we will see an unchanged set of octopuses twice in a row, the fixed point.

We boost the energy as follows:

```haskell
boost :: Octopuses -> Point -> Int -> (Int, Bool)
boost ks k o | k `M.member` ks && o < 10 = let o' = o + lookupFreq k ks in (o', o' > 9)
             | otherwise   = (o, False)
```

If the octopus is in the set of octopuses to boost and the octopus has not yet flashed (`< 10`) then we boost it the required number of times
returning the new energy level and if it just flashed (`> 9`).
If not, we keep the level the same and return that it didn't _just_ flash.

We can then put this into a fixed point and reset the result:

```haskell
step :: Octopuses -> Octopuses
step os = Data.Map.map reset . snd $ fixedPoint next (1 <$ os, os)
    where
        reset o = if o > 9 then 0 else o
```

Here we initialise our frequency map to boost each octopus once, and pass the initial octopus state.
Then we get the final state of the octopuses out of the fixed point and reset each octopus.

That works by setting it's energy level to `0` if it was above `9`.

That's the core logic done.

For part 1 we count the total number of flashes after 100 days:

```haskell
solvea :: Octopuses -> Int
solvea os = (!! 100) . scanl1 (+) . map (countTrue (==0)) . iterate step $ os
```

To do this we keep running the step function, and for each step count the number of `0`s in the octopuses.
`scanl1 (+)` then generates a running total for each step, and finally we get the running total of day 100.

Job done!

For part 2 we need to keep going until all the octopuses flash at the same time, and return the step it happens on:

```haskell
solveb :: Octopuses -> Int
solveb os = fst . head . dropWhile (not . all (==0) . snd) . zip [0..] . iterate step $ os
```

We run the same core logic as last time of iterating the step function on the octopuses,
we then combine each state with it's step index (`zip [0..]`).

The we ignore each state where not all the octopuses have flashed (i.e. reset to `0`).
As soon as we find one (`head`) we take the step index and return it.

Part 2 complete!

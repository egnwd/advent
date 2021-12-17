Day 12 was satisfying as there is a common solution to part 1 and 2 with a tweaked condition (similar to yesterday).

For today, we have a cave network that we need to explore, so the first thing I did was parse the network into a graph.

I won't go into the full graph building logic but the essence is taking the pairs from the inputs and adding them to a graph in both directions,
except when we are coming out of the `"end"` node or into the `"start"` state. If you are curious you can see the `buildGraph` function [here](https://github.com/egnwd/advent/blob/2021/src/AOC/Challenge/Day12.hs#L38).

The important thing to note is the value at each node is if it is a LARGE cave (`True`) or a small cave (`False`).

The output from parsing is a function that takes a node and gives you it's value, key and neighbours in the graph:

```haskell
type Cave = String
type GraphNeighbours = Cave -> (Bool, Cave, [Cave])
```

Our objective is, given some cave entry condition, to find the number of possible paths through the network from `"start"` to `"end"`.

Intuitively, to do this we can follow every allowed neighbour at each node in the graph until it reaches a terminating state,
or the path is pruned because it's not allowed.

We can define a function that takes the predicate and our `getNode` function we parsed.
We keep track internally with the set of nodes to explore next and a frequency map of how many times we have entered a cave on the current path.
The code looks like:

```haskell
findPaths :: EnterCavePredicate -> GraphNeighbours -> Int
findPaths canEnterCave getNode = sum $ findPaths' empty start
    where
        start = getNode "start" ^. _3
        findPaths' _ [] = return 1
        findPaths' seen ns = do
            (large, n, ns') <- map getNode ns
            guard $ canEnterCave seen n large
            let seen' = if large then seen else alter (pure . maybe 1 succ) n seen
            findPaths' seen' ns'
```

`findPaths'` is where the main recursive logic happens,
and we can use the fact that the list is a monad to take full use of the `do` notation.

For recursive functions I always remember a phrase from my lecturer:
> Take a leap of faith

meaning, if you have correctly implemented the current iteration correctly,
you can trust yourself, take a leap of faith and call the function recursivly and get back the right result.

As with all recursive functions we have a base case:
when there are no neighbours to visit (`[]`) then we have reached the `"end"`
(as we removed the paths out of this state when parsing).
Upon terminating we return a `1` to indicate a new path.

For the recursive case we need to branch out from the next set of neighbours `ns`.
We map each neighbour to it's node and get out the `(large, n, ns')` for that node.

If you aren't familiar with `do` notation for lists it acts a little like a list comprehension:

```haskell
f = do
  n <- [1,2,3,4]
  [n*2]
```

is the same as:

```haskell
f = [n * 2 | n <- [1,2,3,4]]
```

Both result in `[2,4,6,8]` note that the return value from the `do` notation is a list,
and the result is the concatenation of those lists.

Back to `findPaths'`:

We can use a neat function from `Control.Monad`: `guard`.

It takes a boolean, which if `True` continues with the execution,
otherwise it short circuits with the `empty` case (`empty` from `Alternative`).

Not surprisingly, `empty` for a list is `[]`. This essentially prunes this branch of the exploration.

If we pass this check we update our seen frequency map (if it's a small cave as that's all we care about,
large caves can always be entered as many times as we like).

_N.B. `alter (pure . maybe 1 succ) n seen` will add to the map the node with the count `1` or if it's already in the map add one (`succ`)._

We then recurse into the neighbours of this node (remember all the neighbours of all the current set to look through will be explored, assuming they aren't pruned).

Finally, we can sum the resulting list of numbers (all 1s), this is the same as getting the length of the list and filling it with `()`.

This works for part 1 and 2, we just need to define the `canEnterCave` predicates:

```haskell
type EnterCavePredicate
    =  M.Map String Int -- ^ Cave -> Number of times visited
    -> Cave             -- ^ Cave name
    -> Bool             -- ^ True when the cave is large
    -> Bool

part1Predicate, part2Predicate :: EnterCavePredicate
part1Predicate seen a large = large || lookupFreq a seen == 0
part2Predicate seen a large = large || (\s -> s == 0 || s == 1 && all (<2) seen) (lookupFreq a seen)
```

In both cases if the cave is LARGE then we short circuit and say we can enter the cave.
For part 1, if we've not seen the cave before then we can enter.
For part 2, if we've not seen it before or we've seen it once, but not seen any other cave more than once, we can enter.

Done!

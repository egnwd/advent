Reading a list of ints always seems to be early on, we can do that easily with:

```haskell
parser = map read . lines
```

Then for part 1 we need to get the differences, which can be achieved by dropping the head of the
list then subtracting the two lists element-wise:

```haskell
differences x = zipWith subtract x (tail x)
```

and then to get the answer we need only the increases we just filter for where we got a postitive result:

```haskell
part1 = length . filter (>0) . differences
```

Yay! 1 star!

For part 2 we can reuse the differences logic from part 1, we just need to construct a new input list using the windows.
The easiest way to do this given the window is only length 3 is use the version of `zipWith` that takes 3 lists, this time with addition.

```haskell
summedSlidingWindows x = zipWith3 (\a b c -> a + b + c) x (drop 1 x) (drop 2 x)
```

Then to solve we just use part 1.

```haskell
part2 = part1 . summedSlidingWindows
```

Day 1 complete!

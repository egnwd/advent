My test cases hurt me today... arguably...

I got the right answer but failed some of the tests,
the rewrite cost me some time, but it is right now...

Anyway, today was a nice "one-liner" in Haskell:

After yesterday's parsing adventure today had... **none!**

so the solution for part 1 is:

```haskell
day06 =
    last                            -- Get the last index (the answer!)
    fst                             -- Get the indicies of the elements
    head                            -- Get first window satisfying the predicate
    . filter ((==4) . size . snd)   -- Filter to those windows of the correct size after deduplicating
    . map
        ( second fromList           -- Make a set out of the elements
        . unzip                     -- Make a tuple of the indicies and the elements
        . take 4)                   -- Force the windows to size 4 (the packet header size)
    . tails                         -- Make list of sliding windows
    . zip [1..]                     -- Add indicies
```

The only functions that might need some extra detail if you aren't familiar:
 - `zip`: take two lists and make a single list of pairwise elements
 - `unzip`: Take a list of tuples (`[(Int, Char)]` for example) and makea tuple of two lists `([Int], [Char])` i.e. the opposite of `zip`
 - `second`: Helper function from Bifunctor that applies a function `f` to the second element of a tuple.

Packet found!

Part 2 involves switching out (both 😴) `4` for `14`.

Ya-ftzzz, Ove-bpzts.

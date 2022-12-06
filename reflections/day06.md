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

#### Wait there is more!

The puzzle clearly says it's a datastream:

> detects a *start-of-packet marker* in the datastream

Therefore we **must** implement it using a streaming library!

Streams are made of composable pipes, here is the type [from the docs](https://mstksg.github.io/conduino/Data-Conduino.html#g:5):

For a `Pipe i o u m a`, you have:

 - `i`: Type of input stream (the things you can `await`)
 - `o`: Type of output stream (the things you `yield`)
 - `u`: Type of the result of the upstream pipe (Outputted when upstream pipe terminates)
 - `m`: Underlying monad (the things you can `lift`)
 - `a`: Result type when pipe terminates (outputted when finished, with `pure` or `return`)

So, now that you've forgotten that:

We can make our pipe for this problem:

```haskell
findPacket s
  = C.sourceList s            -- 1.
  .| C.consecutive 4          -- 2.
  .| C.map toSet              -- 3.
  .| C.filter ((== 4) . size) -- 4.
  .| await                    -- 5.
```

It looks similar to before...

1. We convert the list of signals into a stream
1. Create windows of size 4 from things flowing through the stream
1. Convert each window to a set
1. Filter them down to ones of size 4
1. Await the first element that comes through the stream

Now, unfortunately this gives us the set of unique signals, not the index...
so we need to wrap our pipe in something to track the index.

```haskell
findPacket s
  = C.sourceList s
  .| indexed
     ( C.consecutive 4
    .| C.map toSet
    .| C.filter ((== 4) . size)
    .| await)
  where
    indexed p = C.execStateP 0 (awaitForever (\x -> id += 1 >> yield x) .| p)
```

This `indexed` function awaits each elements, adds 1 to a counter and yields the awaited element.
Then finally outputs the count (i.e. how many elements it processed).

For part two again we just need to replace `4` with `14`.

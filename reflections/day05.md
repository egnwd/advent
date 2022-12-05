Today was a day of tricksy parsing and simple solutions.

I'm targetting the following types to parse into:

```haskell
type Crate = Char
type Ship = IntMap [Crate]
data Instruction = Move !Int !Int !Int
```

Crates are simple `Char`s, `Ship`s are numbered columns of crates.
I'm using normal linked lists for stacks not a proper stack object as it's a small problem and actually some of the manipulations are easier.
Finally, the `Move` instructions are a simple product of number, from column and to column.

First thing we need to parse is the map of crates:

```haskell
parseCrates :: CharParser Ship
parseCrates = fromList . zip [1..] . map catMaybes . transpose <$> pRow
    where
        pRows = some (pCrate <* optional (char ' ')) `sepEndBy1` newline
        pCrate = Just <$> ("[" *> upperChar <* "]") <|> Nothing <$ "   "
```

from bottom to top we have `pCrate` which parses a `Maybe Crate`,
if there is a character in a box you get a `Just c` where `c` is the character,
and if there is no crate (`"   "`) you get `Nothing`.

`pRows` takes one or more newline seperated lines of crates that might be followed by a space (because the last isn't followed by a space).
This returns a `[[Maybe Crate]]` in a row oriented format.
We want columns and we only need the Crates that appear.

For that we transpose the `[[a]]` so that it's column oriented, keep only the `Just` values from each column.
_Note: we have to do this after the transpose to not mess up the columns._
Finally, we zip it into an IntMap using ids `1` onwards,
and thanks to Haskell being lazy we don't need to define an upper bound.

Next we parse the instructions:

```haskell
parsePlan :: CharParser [Instruction]
parsePlan = pPlan `sepEndBy1` newline
    where
        pPlan = Move <$> (labelledN "move") <*> (labelledN "from") <*> (labelledN "to")
        labelledN l = pTok l *> pTok pDecimal
```

again bottom to top, `labelledN` is a way of parsing `<label> <number> `.
A plan is parsed by `pPlan` where we take `move n `, `from f ` and `to t` and use the `Move` constructor.
Finally we take many lines of these plans returning us a `[Instruction]`.

All that is left is to put them together:

```haskell
parsePlanningSheet :: CharParser (Ship, [Instruction])
parsePlanningSheet = do
    s <- parseCrates
    skipManyTill anySingle newline <* newline
    pl <- parsePlan
    return (s, pl)
```

The middle line just skips the row of column numbers as we don't need it.


Now on to the fun bit, solving. Given we have a list of instructions and a starting state we just need a simple `foldl` over the instructions and the starting configuration of crates.

```haskell
moveCrates :: Ship -> Instruction -> Ship
moveCrates s (Move n from to)
    = adjust (moving ++) to -- add the list of crates to the to column
    . adjust (drop n) from  -- remove the crates from the from column
    $ s                     -- current ship
    where
        moving = reverse . take n $ s ! f
```

here we have a move function that will take off the `n` items (and reverse as we take off 1 by 1).

Then we just need to run each instruction and take the top element from the stacks.

```haskell
day05a = map head . foldl' moveCrates . parseOrFail parsePlanningSheet
```

Yay! A star!

For part two it's even easier! We just don't reverse the list as we grab all of them.

```haskell
moveCrates9001 :: Ship -> Instruction -> Ship
moveCrates9001 s (Move n from to)
    = adjust (moving ++) to -- add the list of crates to the to column
    . adjust (drop n) from  -- remove the crates from the from column
    $ s                     -- current ship
    where
        moving = take n $ s ! f

day05b = map head . foldl' moveCrates9001 . parseOrFail parsePlanningSheet
```

Done!

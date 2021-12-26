Another growth issue to contend with, but we can solve it in a similar manner to the lanternfish,
keeping track of a frequency map of states to reduce the number of operations we need to perform.

Luckily part 1 & 2 have common solution.

First let's set up the skeleton of the process, we need to input a formula and get the range of frequencies out.

```haskell
type Element = Char

formulaFrequencies :: Map (Element, Element) Int -> Map Element Int
formulaFrequencies = M.mapKeysWith (+) snd

scoreFormula :: (Num n, Ord n) => Map a n -> Maybe n
scoreFormula f
  = case (maximumOf traverse f, minimumOf traverse f) of
    (Just mx, Just mn) -> Just (mx - mn)
    _                    -> Nothing

solve :: Int -> (String, Map (Element, Element) Element) -> Maybe Int
solve n = scoreFormula . formulaFrequencies <=< uncurry (generateFormula n)
```

`generateFormula` will take an initial formula and give back a map of element pair frequencies after `n` steps.
`formulaFrequencies` take the steps and returns just the second element from each pair.
This is to avoid repeated counts for the elements.
_I'm pretty sure this nly works because the initial letter of my input formular is neither the minimum or maximum, but oh well._

`scoreFormula` then gets the range of the frequencies, assuming they exist.

The core then happens in `generateFormula`.

We iterate a substituion step n times and return the final frequencies.

```haskell
generateFormula :: Int -> String -> FormulaMapping -> Maybe (FormulaFrequencies (Element, Element))
generateFormula n formula m = (!? n) . iterate substitute $ formula'
    where
        formula' = freqs . pairwise $ formula
        pairwise ls = zip ls (tail ls)
        substitute = M.fromListWith (+) . foldMap (\(k,a) -> map (,a) . new $ k) . M.toList
        new k@(a,b) = case M.lookup k m of { Just c -> [(a,c), (c,b)]; Nothing -> [k] }
```

The `substitute` step takes each pair and runs a substituion phase which produces up to 2 pairs,
for example given the pair `(NV, 4)` and the substituion `NV -> C` we get `[(NC, 4), (CV, 4)]`.

We then roll up the frequencies into another frequency map.

`n` steps later and we have the answer!

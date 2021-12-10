{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day10 (
    day10a
  , day10b
  ) where

import           AOC.Prelude

isOpen = (`elem` "({[<")

matching = \case
  "()" -> True
  "[]" -> True
  "{}" -> True
  "<>" -> True
  _    -> False

score = \case
    ')' -> Sum 3
    ']' -> Sum 57
    '}' -> Sum 1197
    '>' -> Sum 25137
    _   -> Sum 0

scoreb = \case
    '(' -> 1
    '[' -> 2
    '{' -> 3
    '<' -> 4
    _   -> 0

solve [] = Nothing
solve (b:bs) = solve' bs [b]
    where
        solve' [] _ = Nothing
        solve' (_:_) [] = Nothing
        solve' (b':bs') (o:os) | isOpen b'        = solve' bs' (b':o:os)
                               | matching [o, b'] = solve' bs' os
                               | otherwise        = Just $ score b'

solveb [] = Nothing
solveb (b:bs) = solve' bs [b]
    where
        solve' _ [] = Nothing
        solve' [] os = Just $ foldl (\s c -> (s*5) + scoreb c) 0 os
        solve' (b':bs') (o:os) | isOpen b'        = solve' bs' (b':o:os)
                               | matching [o, b'] = solve' bs' os
                               | otherwise        = Nothing

middle ns = ns !? (length ns `div` 2)

day10a :: _ :~> _
day10a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = fmap getSum . foldMap solve
    }

day10b :: _ :~> _
day10b = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = middle . sort . mapMaybe solveb
    }

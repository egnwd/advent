{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day08 (
    day08a
  , day08b
  ) where

import           AOC.Prelude
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M

splitOn c = map T.unpack . T.splitOn c . T.pack

parser = map (map words . splitOn "|") . lines

outputs [_, o] = o
signals [s, _] = s

predi = (`elem` [2,3,4,7]) . length

solve = countTrue predi . concatMap outputs

zero  = "abcefg"
one   = "cf"
two   = "acdeg"
three = "acdfg"
four  = "bcdf"
five  = "abdfg"
six   = "abdefg"
seven = "acf"
eight = "abcdefg"
nine  = "abcdfg"

digits = map S.fromList [zero, one, two, three, four, five, six, seven, eight, nine]

solveb ds = sum $ zipWith (\r m -> numberFromDigits . map (pickNumber . S.fromList . map (m M.!)) $ outputs r) ds answers
    where
        uniqueChoices = map (pickUnique . M.toList . M.fromListWith S.intersection . choices . signals) ds
        answers = zipWith (\r ms -> head . filter (\m -> all (\s -> S.fromList (map (m M.!) s) `elem` digits) $ signals r) $ ms) ds uniqueChoices

pickNumber s = pickNumber' digits 0
    where
        pickNumber' (d:ds) n | d == s = n
                             | otherwise = pickNumber' ds (n+1)

numberFromDigits = foldl (\n d -> n * 10 + d) 0

choices :: [String] -> [(Char, S.Set Char)]
choices ss = concatMap choices' ss
    where
        choices' s | length s == 2 = map (, S.fromList one) s
                   | length s == 3 = map (, S.fromList seven) s
                   | length s == 4 = map (, S.fromList four) s
                   | length s == 7 = map (, S.fromList eight) s
                   | otherwise = []

day08a :: _ :~> _
day08a = MkSol
    { sParse = Just . parser
    , sShow  = show
    , sSolve = Just . solve
    }

day08b :: _ :~> _
day08b = MkSol
    { sParse = Just . parser
    , sShow  = show
    , sSolve = Just . solveb
    }

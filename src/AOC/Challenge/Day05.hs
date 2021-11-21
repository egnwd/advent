-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.

module AOC.Challenge.Day05 (
    day05a
  , day05b
  ) where

import AOC.Solver
import AOC.Common
import Data.List

nice :: String -> Bool
nice w = noBadSets && atLeastTwoVowels && doubleLetter w
  where
    atLeastTwoVowels = countTrue (`elem` "aeiou") w >= 3
    doubleLetter (x : y : _) | x == y = True
    doubleLetter (_ : xs) = doubleLetter xs
    doubleLetter [] = False
    noBadSets = not . any (`isInfixOf` w) $ ["ab", "cd", "pq", "xy"]

nice2 :: String -> Bool
nice2 w = hasRepeat w && doublePair w
  where
    hasRepeat (x : _ : y : _) | x == y = True
    hasRepeat (_ : xs) = hasRepeat xs
    hasRepeat [] = False

    doublePair (x : y : xs) | [x,y] `isInfixOf` xs = True
    doublePair (_ : xs) = doublePair xs
    doublePair [] = False

day05a :: [String] :~> Int
day05a = MkSol
  { sParse = parseLines pWord
  , sShow  = show
  , sSolve = Just . countTrue nice
  }

day05b :: [String] :~> Int
day05b = MkSol
  { sParse = parseLines pWord
  , sShow  = show
  , sSolve = Just . countTrue nice2
  }

-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import AOC.Solver      ((:~>)(..))
import AOC.Common      (countTrue)
import Data.List       (transpose, sort)
import Data.List.Split (chunksOf)
import Text.Read       (readMaybe)

solve :: [[Int]] -> Int
solve = countTrue (\case [a,b,c] -> a+b > c; _ -> False)

day03a :: [[Int]] :~> Int
day03a = MkSol
    { sParse = traverse (fmap sort . traverse (readMaybe @ Int) . words). lines
    , sShow  = show
    , sSolve = Just . solve
    }

day03b :: [[Int]] :~> Int
day03b = MkSol
    { sParse = fmap (fmap sort . concatMap (chunksOf 3) . transpose) . traverse (traverse (readMaybe @ Int) . words). lines
    , sShow  = show
    , sSolve = Just . solve
    }

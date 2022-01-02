-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.

module AOC.Challenge.Day01 (
    day01a
  , day01b
  ) where

import AOC.Solver ((:~>)(..))
import Data.Char (digitToInt, isDigit)

parser :: String -> Maybe [Int]
parser = traverse (pure . digitToInt) . filter isDigit

solve :: Int -> [Int] -> Int
solve n = sum . (zipWith (\a b -> if a == b then a else 0) <*> (drop n . cycle))

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . solve 1
    }

day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = \is -> Just $ solve (length is `div` 2) is
    }

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
import AOC.Common (countTrue)
import Text.Read  (readMaybe)

parser :: String -> Maybe [Int]
parser = traverse readMaybe . lines

solve :: Int -> [Int] -> Int
solve w = countTrue (>0) . (zipWith subtract <*> drop w)

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
    , sSolve = Just . solve 3
    }

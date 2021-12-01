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

solve :: [Int] -> Int
solve x = countTrue (>0) $ zipWith subtract x (drop 1 x)

summedSlidingWindows :: (Num a) => [a] -> [a]
summedSlidingWindows x = zipWith (+) (zipWith (+) x (drop 1 x)) (drop 2 x)

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . solve
    }

day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . solve . summedSlidingWindows
    }

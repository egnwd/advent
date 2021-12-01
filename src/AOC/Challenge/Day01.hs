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

parser :: String -> [Int]
parser = map read . lines

solve :: [Int] -> Int
solve x = countTrue (>0) $ zipWith subtract x (drop 1 x)

addToWindow :: (Num a) => [a] -> [a] -> [a]
addToWindow w x = zipWith (+) w (drop 1 x)

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = Just . parser
    , sShow  = show
    , sSolve = Just . solve
    }

day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = Just . parser
    , sShow  = show
    , sSolve = \x -> Just . solve $ addToWindow (addToWindow x x) (drop 1 x)
    }

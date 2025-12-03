-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import AOC.Solver    ((:~>)(..))
import Data.Ord      (comparing)
import Data.List     (maximumBy)
import Data.Char     (digitToInt)
import Data.Monoid   (Sum(..))
import Control.Arrow ((&&&))

maxItem :: Int -> [Int] -> Int
maxItem 1 xs = maximum xs
maxItem n xs = let searchSpace = length xs - n + 1
                   firstLargest = snd &&& (negate . fst)
                   (from, j1) = maximumBy (comparing firstLargest) . zip [0..] . take searchSpace $ xs
                in (j1 * 10 ^ (n-1)) + maxItem (n - 1) (drop (from + 1) xs)

day03 :: Int -> [[Int]] :~> Int
day03 n = MkSol
    { sParse = Just . map (map digitToInt) . lines
    , sShow  = show
    , sSolve = Just . getSum . foldMap (Sum . maxItem n)
    }

day03a :: [[Int]] :~> Int
day03a = day03 2

day03b :: [[Int]] :~> Int
day03b = day03 12

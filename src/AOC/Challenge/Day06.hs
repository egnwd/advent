-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day06 (
    day06a
  , day06b
  ) where

import AOC.Solver        ((:~>)(..))
import AOC.Common        (listTup)
import AOC.Common.Search (binarySearch)
import Text.Read         (readMaybe)
import Control.Monad     (liftM2, (<=<))

findMinTimeExcl :: Int -> Int -> Int
findMinTimeExcl t mx = binarySearch 0 t p
    where
        p d = d * (t-d) > mx

findPossibilities :: Int -> Int -> Int
findPossibilities n lo = (n+1) - (lo * 2)

findSolution :: Int -> Int -> Int
findSolution = liftM2 (.) findPossibilities findMinTimeExcl

day06a :: [(Int, Int)] :~> Int
day06a = MkSol
    { sParse = fmap (uncurry zip) . listTup <=< traverse (traverse (readMaybe @ Int) . drop 1 . words) .  lines
    , sShow  = show
    , sSolve = Just . product . map (uncurry findSolution)
    }

day06b :: (Int, Int) :~> Int
day06b = MkSol
    { sParse = listTup <=< traverse (readMaybe @ Int . filter (' ' /=) . unwords . drop 1 . words) .  lines
    , sShow  = show
    , sSolve = Just . uncurry findSolution
    }

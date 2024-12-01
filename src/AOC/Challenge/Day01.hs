-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day01 (
    day01a
  , day01b
  ) where

import           AOC.Solver                     ((:~>)(..))
import           AOC.Common                     (freqs, lookupFreq, listTup)
import           Data.List.Split                (splitOn)
import           Data.List                      (sort)
import           Text.Read                      (readMaybe)
import           Control.Monad                  ((<=<))

solvea :: [Int] -> [Int] -> [Int]
solvea (sort->a) (sort->b) = zipWith score a b
    where
        score x y = abs $ x - y

solveb :: [Int] -> [Int] -> [Int]
solveb a (freqs->b) = map sim a
    where
        sim x = x * lookupFreq x b

day01 :: ([Int] -> [Int] -> [Int]) -> ([Int], [Int]) :~> Int
day01 solve = MkSol
    { sParse = fmap unzip . traverse (listTup <=< traverse readMaybe . splitOn "  ") . lines
    , sShow  = show
    , sSolve = Just . sum . uncurry solve
    }

day01a :: ([Int], [Int]) :~> Int
day01a = day01 solvea

day01b :: ([Int], [Int]) :~> Int
day01b = day01 solveb

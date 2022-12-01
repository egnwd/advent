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

import Data.List.Split (splitOn)
import Text.Read       (readMaybe)
import AOC.Solver      ((:~>)(..))
import Data.List       (sortBy)
import Data.Ord        (Down)

type Calories = Int
type Elf = [Calories]

findTopCalorificElves :: Int -> [Elf] -> Calories
findTopCalorificElves n = sum . take n . sortBy (comparing Down) . map sum

parseElves :: String -> Maybe [Elf]
parseElves = traverse (traverse readMaybe) . map lines . splitOn "\n\n"

day01a :: [Elf] :~> Calories
day01a = MkSol
    { sParse = parseElves
    , sShow  = show
    , sSolve = Just . findTopCalorificElves 1
    }

day01b :: [Elf] :~> Calories
day01b = MkSol
    { sParse = parseElves
    , sShow  = show
    , sSolve = Just . findTopCalorificElves 3
    }

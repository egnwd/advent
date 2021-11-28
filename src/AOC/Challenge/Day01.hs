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
import Data.Monoid (Sum(..))

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = traverse getValue
    , sShow  = show
    , sSolve = Just . sum
    }

day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = traverse getValue
    , sShow  = show
    , sSolve = Just . getSum . snd . foldl solveb (0,0)
    }

getValue :: Char -> Maybe Int
getValue '(' = Just 1
getValue ')' = Just (-1)
getValue _   = Nothing

solveb :: (Sum Int, Sum Int) -> Int -> (Sum Int, Sum Int)
solveb p@(-1, _) _ = p
solveb p c = (Sum c, Sum 1) <> p

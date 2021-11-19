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
import Data.Bifunctor (bimap)

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = Just . map getValue
    , sShow  = show
    , sSolve = Just . sum
    }

day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = Just . map getValue
    , sShow  = show
    , sSolve = Just . snd . foldl solveb (0,0)
    }

getValue :: Char -> Int
getValue '(' = 1
getValue ')' = -1
getValue _ = undefined

solveb :: (Int, Int) -> Int -> (Int, Int)
solveb p@(-1, _) _ = p
solveb p c = bimap (+c) (+1) p

-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import           AOC.Solver   ((:~>) (..))
import           Control.Lens ((^?), _head)
import           Text.Read    (readMaybe)

parser :: String -> Maybe [[Int]]
parser = traverse (traverse (readMaybe @ Int) . words) . lines

solve :: [Int] -> Int
solve = (-) <$> maximum <*> minimum

solveb :: [Int] -> Maybe Int
solveb xs = [d | a <- xs, b <- xs, a /= b, let (d,m) = a `divMod` b, m == 0] ^? _head

day02a :: [[Int]] :~> Int
day02a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . sum . map solve
    }

day02b :: [[Int]] :~> Int
day02b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = fmap sum . traverse solveb
    }

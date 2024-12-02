-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import           AOC.Solver          ((:~>)(..))
import           AOC.Common          (countTrue, (&&&), (|||))
import           Text.Read           (readMaybe)
import           Data.Ix             (inRange)
import           Data.List           (inits, tails)
import           Control.Applicative (liftA2)

rule :: (a -> a -> Bool) -> [a] -> Bool
rule p xs = all (uncurry p) $ zip xs (tail xs)

rules :: [Int] -> Bool
rules = monotonic &&& notTooMuch
    where
        monotonic = rule (>) ||| rule (<)
        notTooMuch = rule $ \a b -> inRange (1,3) (abs $ a - b)

parse :: String -> Maybe [[Int]]
parse = traverse (traverse readMaybe . words) . lines

day02a :: [[Int]] :~> Int
day02a = MkSol
    { sParse = parse
    , sShow  = show
    , sSolve = Just . countTrue rules
    }

day02b :: [[Int]] :~> Int
day02b = MkSol
    { sParse = parse
    , sShow  = show
    , sSolve = Just . countTrue (any rules . liftA2 (zipWith (++)) inits (tail . tails))
    }

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

import AOC.Solver      ((:~>)(..))
import Data.List       (transpose)
import Data.List.Split (splitOn, splitWhen)
import Text.Read       (readMaybe)
import Control.Arrow   (second)
import Control.Monad   ( (>=>) )

parse :: [String] -> Maybe ([Int] -> Int, [Int])
parse ("+":xs) = sequence (sum, traverse readMaybe xs)
parse ("*":xs) = sequence (product, traverse readMaybe xs)
parse _ = Nothing

getOp :: [String] -> Maybe (Char, [String])
getOp [] = Nothing
getOp [x] = Just (last x, [init x])
getOp (x:xs) = second (x :) <$> getOp xs

parse2 :: [String] -> Maybe ([Int] -> Int, [Int])
parse2 = getOp >=> \case
    ('+', ys) -> sequence (sum, traverse readMaybe ys)
    ('*', ys) -> sequence (product, traverse readMaybe ys)
    _ -> Nothing

day06a :: [([Int] -> Int, [Int])] :~> Int
day06a = MkSol
    { sParse = traverse (parse . reverse) . transpose . map (filter (not . null) . splitOn " ") . lines
    , sShow  = show
    , sSolve = Just . sum . map (uncurry ($))
    }

day06b :: [([Int] -> Int, [Int])] :~> Int
day06b = MkSol
    { sParse = traverse (parse2 . reverse) . splitWhen (all (==' ')) .  transpose . lines
    , sShow  = show
    , sSolve = Just . sum . map (uncurry ($))
    }

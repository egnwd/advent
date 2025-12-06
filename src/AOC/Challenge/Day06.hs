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
import Data.Char       (isSpace)
import Data.List       (transpose, uncons)
import Data.List.Split (splitWhen)
import Text.Read       (readMaybe)
import Control.Arrow   (second)
import Control.Monad   ((>=>), (<=<))

parse :: String -> [String] -> Maybe Int
parse "+" xs = sum <$> traverse readMaybe xs
parse "*" xs = product <$> traverse readMaybe xs
parse _ _ = Nothing

getOp :: [String] -> Maybe (Char, [String])
getOp [] = Nothing
getOp [x] = Just (last x, [init x])
getOp (x:xs) = second (x :) <$> getOp xs

parse2 :: [String] -> Maybe Int
parse2 = getOp >=> \case
    ('+', ys) -> sum <$> traverse readMaybe ys
    ('*', ys) -> product <$> traverse readMaybe ys
    _ -> Nothing

day06a :: [Int] :~> Int
day06a = MkSol
    { sParse = traverse (uncurry parse <=< uncons . reverse) . transpose . map words . lines
    , sShow  = show
    , sSolve = Just . sum
    }

day06b :: [Int] :~> Int
day06b = MkSol
    { sParse = traverse (parse2 . reverse) . splitWhen (all isSpace) .  transpose . lines
    , sShow  = show
    , sSolve = Just . sum
    }

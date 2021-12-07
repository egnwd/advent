{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.

module AOC.Challenge.Day07 (
    day07a
  , day07b
  ) where

import AOC.Solver               ((:~>)(..))
import Text.Read                (readMaybe)
import Data.Semigroup           (Min(..))
import qualified Data.Text as T

parser :: String -> Maybe [Int]
parser = traverse (readMaybe . T.unpack) . T.splitOn "," . T.pack

parta, partb :: Int -> Int
parta = id
partb x = (x * (x+1)) `div` 2

score :: (Int -> Int) -> [Int] -> Int -> Int
score f cs c = sum . map (f . abs . subtract c) $ cs

solve :: (Int -> Int) -> [Int] -> Maybe Int
solve f cs = fmap getMin . foldMap (Just . Min . score f cs) $ [minimum cs .. maximum cs]

day07a :: [Int] :~> Int
day07a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = solve parta
    }

day07b :: [Int] :~> Int
day07b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = solve partb
    }

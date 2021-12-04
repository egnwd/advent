{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.

module AOC.Challenge.Day04 (
    day04a
  , day04b
  ) where

import AOC.Solver    ((:~>)(..))
import Data.Function (on)
import Control.Lens  (mapped, (<&>), _head, (%~), preview)
import Data.List     (partition, transpose)
import Data.Either   (isLeft)
import qualified Data.Text as T

type BingoCard = [[Either Int Int]]

parse :: String -> ([Int], [BingoCard])
parse s = let (x:_:xs) = lines s
           in (map (read . T.unpack) (T.splitOn "," (T.pack x)), parseBingoCards (unlines xs))

parseBingoCards :: String -> [BingoCard]
parseBingoCards xs = let bs = T.splitOn "\n\n" (T.pack xs)
                       in map parseBingoCard bs

parseBingoCard :: T.Text -> BingoCard
parseBingoCard = map (map (Right . read @Int) . words) . lines . T.unpack

solve :: ([BingoCard] -> [BingoCard] -> Bool) -> [Int] -> [BingoCard] -> Maybe Int
solve _ [] _ = Nothing
solve finished (n:ns) bs = if finished bs w then scoreFirstBoard w else solve finished ns l
    where
        scoreBoard b = (sum . unmarked $ b) * n
        unmarked = map (either (const 0) id) . concat
        scoreFirstBoard = fmap scoreBoard . preview _head
        (w,l) = partition winner newboards
        newboards = bs <&> mapped.mapped %~ mark
        mark (Right x) | x == n = Left x
        mark x = x

winner :: BingoCard -> Bool
winner bs = row bs || column bs
    where
        column = row . transpose
        row = any (all isLeft)

day04a :: ([Int], [[[Either Int Int]]]) :~> _
day04a = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = uncurry $ solve (const (not . null))
    }

day04b :: _ :~> _
day04b = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = uncurry $ solve ((==) `on` length)
    }

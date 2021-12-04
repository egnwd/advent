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

import AOC.Solver     ((:~>)(..))
import Data.Semigroup (First(..), Last(..), getFirst, getLast)
import Data.List      (transpose, partition)
import Control.Lens   (mapped, (<&>), (%~))
import qualified Data.Text as T

data BingoNumber = Unmarked Int | Marked Int deriving (Eq, Show)
type BingoCard = [[BingoNumber]]

unmarkedValue :: BingoNumber -> Int
unmarkedValue (Unmarked n) = n
unmarkedValue (Marked _)   = 0

isMarked :: BingoNumber -> Bool
isMarked = \case
    Marked _ -> True
    _ -> False

parse :: String -> ([Int], [BingoCard])
parse s = let (x:_:xs) = lines s
           in (map (read . T.unpack) (T.splitOn "," (T.pack x)), parseBingoCards (unlines xs))

parseBingoCards :: String -> [BingoCard]
parseBingoCards xs = let bs = T.splitOn "\n\n" (T.pack xs)
                       in map parseBingoCard bs

parseBingoCard :: T.Text -> BingoCard
parseBingoCard = map (map (Unmarked . read @Int) . words) . lines . T.unpack

solve' :: (Semigroup s) => (Int -> s) -> [Int] -> [BingoCard] -> Maybe s
solve' _ [] _ = Nothing
solve' f (n:ns) bs = winners <> solve' f ns l
    where
        winners = foldMap (Just . f . scoreBoard) w
        (w,l) = partition winner bs'
        scoreBoard b = (sum . unmarked $ b) * n
        unmarked = map unmarkedValue . concat
        bs' = bs <&> mapped.mapped %~ mark
        mark (Unmarked x) | x == n = Marked x
        mark x = x

winner :: BingoCard -> Bool
winner bs = row bs || column bs
    where
        column = row . transpose
        row = any (all isMarked)

day04a :: ([Int], [BingoCard]) :~> _
day04a = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = fmap getFirst . uncurry (solve' First)
    }

day04b :: ([Int], [BingoCard]) :~> _
day04b = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = fmap getLast . uncurry (solve' Last)
    }

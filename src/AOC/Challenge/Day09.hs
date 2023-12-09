-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day09 (
    day09a
  , day09b
  ) where

import           AOC.Solver ((:~>)(..))

import           Text.Read (readMaybe)
import           Data.Vector.Unboxed (Vector)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty             as NE
import qualified Data.Vector.Unboxed            as V

extrapolate :: (b -> b -> b) -> (Vector Int -> b) -> Vector Int -> Maybe b
extrapolate combine choose = fmap (foldr1 combine . fmap choose) . findDifferences
    where
        findDifferences :: Vector Int -> Maybe (NonEmpty (Vector Int))
        findDifferences = NE.nonEmpty . takeWhile (not . V.all (==0)) . iterate (V.zipWith subtract <$> id <*> V.tail)

day09 :: (Int -> Int -> Int) -> (Vector Int -> Int) -> [Vector Int] :~> Int
day09 combine choose = MkSol
    { sParse = fmap (map V.fromList) . traverse (traverse readMaybe . words) . lines
    , sShow  = show
    , sSolve = fmap sum . traverse (extrapolate combine choose)
    }

day09a :: [Vector Int] :~> Int
day09a = day09 (+) V.last

day09b :: [Vector Int] :~> Int
day09b = day09 (-) V.head

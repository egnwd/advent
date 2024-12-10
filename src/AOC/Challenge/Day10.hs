-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day10 (
    day10a
  , day10b
  ) where

import           AOC.Solver     ((:~>)(..))
import           AOC.Common     (parseAsciiMap, decDigit, Point, neighboursSet)

import           Control.Lens   (preview)
import           Data.Finite    (Finite)
import           Data.Map       (Map)
import           Data.Semigroup (Sum(..))
import qualified Data.Map       as M
import qualified Data.Set       as S

next :: Map Point (Finite 10) -> Finite 10 -> Point -> [Point]
next mp a p = M.keys . M.filter (== succ a) $ mp `M.restrictKeys` neighboursSet  p

searchTrailheads :: (Monoid a, Num b) => (Point -> a) -> (a -> Sum b) -> Map Point (Finite 10) -> b
searchTrailheads found score mp = getSum . foldMap (score . buildTrail 0) $ M.keys . M.filter (== 0) $ mp
    where
        buildTrail x p
          | x == 9 = found p
          | otherwise = foldMap (buildTrail (succ x)) $ next mp x p

day10 :: (Monoid a, Num b, Show b) => (Point -> a) -> (a -> Sum b) -> Map Point (Finite 10) :~> b
day10 found score = MkSol
    { sParse = Just . parseAsciiMap (preview decDigit)
    , sShow  = show
    , sSolve = Just . searchTrailheads found score
    }

day10a :: Map Point (Finite 10) :~> Int
day10a = day10 S.singleton (Sum . S.size)

day10b :: Map Point (Finite 10) :~> Int
day10b = day10 (const 1) id

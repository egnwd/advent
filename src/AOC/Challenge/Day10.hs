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

import           AOC.Solver ((:~>)(..))
import           AOC.Common (parseAsciiMap, decDigit, Point, neighboursSet)

import           Data.Map                       (Map)
import qualified Data.Map                       as M
import qualified Data.Set                       as S
import Data.Finite (Finite)
import Control.Lens (preview)
import Data.Semigroup (Sum(..))

next :: Map Point (Finite 10) -> Finite 10 -> Point -> [Point]
next mp a p = M.keys . M.filter (== succ a) $ mp `M.restrictKeys` neighboursSet  p

searchTrailheads :: (Monoid a, Num b) => (Point -> a) -> (a -> Sum b) -> Map Point (Finite 10) -> b
searchTrailheads found score mp = getSum . foldMap (score . buildTrail 0) $ M.keys . M.filter (== 0) $ mp
    where
        buildTrail x p
          | x == 9 = found p
          | otherwise = foldMap (buildTrail (succ x)) $ next mp x p

day10a :: Map Point (Finite 10) :~> Int
day10a = MkSol
    { sParse = Just . parseAsciiMap (preview decDigit)
    , sShow  = show
    , sSolve = Just . searchTrailheads S.singleton (Sum . S.size)
    }

day10b :: Map Point (Finite 10) :~> Int
day10b = MkSol
    { sParse = Just . parseAsciiMap (preview decDigit)
    , sShow  = show
    , sSolve = Just . searchTrailheads (const 1) id
    }

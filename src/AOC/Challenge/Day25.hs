-- |
-- Module      : AOC.Challenge.Day25
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 25.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day25 (
    day25a
  , day25b
  ) where

import           AOC.Common (Point, countTrue, parseAsciiSet, boundingBox)
import           AOC.Solver ((:~>)(..))

import           Data.List         (partition)
import           Data.List.Split   (splitOn)
import           Data.Set.NonEmpty (NESet, withNonEmpty, disjoint, nonEmptySet, isSubsetOf)
import           Linear (V2(..))
import qualified Data.Set          as S

findLocksAndKeys :: [NESet Point] -> ([NESet Point], [NESet Point])
findLocksAndKeys = partition isLock
    where
        isLock s = let V2 (V2 mnx mny) (V2 mxx _) = boundingBox s
                    in withNonEmpty False (`isSubsetOf` s) $ S.fromList [V2 x mny | x <- [mnx..mxx]]

solve :: [NESet Point] -> Int
solve schematics = countTrue id $ overlaps <$> locks <*> keys
    where
        overlaps = disjoint
        (locks, keys) = findLocksAndKeys schematics

day25a :: _ :~> _
day25a = MkSol
    { sParse = traverse (nonEmptySet . parseAsciiSet (== '#')) . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . solve
    }

day25b :: _ :~> _
day25b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

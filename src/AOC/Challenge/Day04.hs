-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day04 (
    day04a
  , day04b
  ) where

import           AOC.Common (allNeighboursSet, allDirSet, parseAsciiMap, Point, Dir(..), orientDir)
import           AOC.Solver ((:~>)(..))

import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.Set                       (Set)
import qualified Data.Set                       as S
import qualified Data.Set.NonEmpty              as NES
import           Linear                         (V2(..))
import           Data.Tuple                     (swap)
import           Control.Arrow                  (first)

letterToPoint :: Map Point Char -> Map Char (Set Point)
letterToPoint = M.fromListWith (<>)
              . map (swap . first S.singleton)
              . M.toList

checkForXmas :: Char -> (Point -> a -> Map Point Char) -> Set a -> Map Point Char -> Maybe (Set (Map Point Char))
checkForXmas seed construct dirs m = expand construct dirs m <$> M.lookup seed (letterToPoint m)

expand :: (Point -> a -> Map Point Char) -> Set a -> Map Point Char -> S.Set Point -> Set (Map Point Char)
expand construct dirs m = S.unions . S.map go
    where
        go x = S.filter (`M.isSubmapOf` m) . S.map (construct x) $ dirs

constructXmas :: Point -> Point -> Map Point Char
constructXmas x d = M.fromList [(x, 'X'), (d + x, 'M'), (2 * d + x, 'A'), (3 * d + x, 'S')]

constructX'mas :: Point -> Dir -> Map Point Char
constructX'mas m d = M.mapKeys (\k -> orientDir (d, k) + m) xmas
    where
        xmas = M.fromList [(V2 0 0, 'M'), (V2 1 1, 'A'), (V2 2 2, 'S'), (V2 0 2, 'M'), (V2 2 0, 'S')]

day04a :: Map Point Char :~> Int
day04a = MkSol
    { sParse = Just. parseAsciiMap Just
    , sShow  = show
    , sSolve = fmap length . checkForXmas 'X' constructXmas (allNeighboursSet (V2 0 0))
    }

day04b :: Map Point Char :~> Int
day04b = MkSol
    { sParse = sParse day04a
    , sShow  = show
    , sSolve = fmap length . checkForXmas 'A' constructX'mas (NES.toSet allDirSet)
    }

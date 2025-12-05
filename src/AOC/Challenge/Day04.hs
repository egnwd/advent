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

import           AOC.Common (parseAsciiSet, allNeighboursSet, fixedPoint, countTrue, Point)
import           AOC.Solver  ((:~>)(..))

import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.Set                       (Set)
import qualified Data.Set                       as S
import           Data.List                      (foldl')

has4Adjacent :: Map Point (Set Point) -> Point -> Bool
has4Adjacent ps p = (<4) . S.size $ ps M.! p

getNeighbours :: Set Point -> Map Point (Set Point)
getNeighbours ps = M.map (S.intersection ps) $ M.fromSet allNeighboursSet ps

removeRoll :: Map Point (Set Point) -> Point -> Map Point (Set Point)
removeRoll neighs p = foldl' (flip (M.adjust (S.delete p))) (M.delete p neighs) (neighs M.! p)

part2 :: Set Point -> Set Point
part2 ps0 = M.keysSet $ fixedPoint go ns
    where
        ns = getNeighbours ps0
        go :: Map Point (Set Point) -> Map Point (Set Point)
        go ps = foldl' removeRoll ps toRemove
            where
                toRemove = filter (has4Adjacent ps) $ M.keys ps

day04a :: _ :~> _
day04a = MkSol
    { sParse = Just . parseAsciiSet (=='@')
    , sShow  = show
    , sSolve = \ps -> Just $ countTrue (has4Adjacent (getNeighbours ps)) ps
    }

day04b :: _ :~> _
day04b = MkSol
    { sParse = sParse day04a
    , sShow  = show
    , sSolve =  \ps0 -> Just . S.size . (ps0 `S.difference`) $ part2 ps0
    }

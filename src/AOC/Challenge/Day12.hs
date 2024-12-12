-- |
-- Module      : AOC.Challenge.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day12 (
    day12a
  , day12b
  ) where

import           AOC.Solver ((:~>)(..))
import           AOC.Common (contiguousRegions, Point, neighboursSet, Dir(..), parseAsciiMap, dirVec)

import           Data.Map                       (Map)
import           Data.Set                       (Set)
import           Data.Set.NonEmpty              (NESet)
import qualified Data.Map                       as M
import qualified Data.Set                       as S
import qualified Data.Set.NonEmpty              as NES

splitMaps :: Map Point Char -> Map Char (Set Point)
splitMaps = M.fromListWith S.union . map (\(a,b) -> (b, S.singleton a)) . M.toList

calculatePrice :: (NESet Point -> Int) -> Set (NESet Point) -> Int
calculatePrice p = sum . map p . S.toList

price :: NESet Point -> Int
price = (*) <$> perim <*> area
    where
        perim (NES.toSet->x) = sum . map (S.size . (`S.difference` x) . neighboursSet) . S.toList $ x
        area = NES.size

priceDiscount :: NESet Point -> Int
priceDiscount = (*) <$> perim <*> area
    where
        perim (NES.toSet->x) = sum $ map go [ North .. ]
            where
                go d = S.size . contiguousRegions . foldMap ((`S.difference` x) . S.singleton . (+dirVec d)) $ x
        area = NES.size

day12 :: (NESet Point -> Int) -> Map Point Char :~> Int
day12 p = MkSol
    { sParse = Just . parseAsciiMap Just
    , sShow  = show
    , sSolve = Just . sum . M.map (calculatePrice p . contiguousRegions) . splitMaps
    }

day12a :: Map Point Char :~> Int
day12a = day12 price

day12b :: Map Point Char :~> Int
day12b = day12 priceDiscount

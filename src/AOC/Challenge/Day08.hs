-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day08 (
    day08a
  , day08b
  ) where

import           AOC.Solver ((:~>)(..))
import           AOC.Common (Point, inBoundingBox, boundingBox, parseAsciiMap)

import           Control.Monad                  (guard)
import           Data.Functor                   (($>))
import           Data.List                      (unfoldr)
import           Data.Map                       (Map)
import           Data.Set                       (Set)
import qualified Data.Map                       as M
import qualified Data.Set                       as S
import qualified Data.Set.NonEmpty              as NES

data Airspace = Air | Antenna Char deriving Eq

type AntennaRule = (Point -> Bool) -> (Point, Point) -> [Point]

parse :: Char -> Maybe Airspace
parse '.' = Just Air
parse c = Just (Antenna c)

brokenRule, correctRule :: AntennaRule
brokenRule inMap (a, b) = let next = b + b - a
                           in guard (inMap next) $> next

correctRule inMap = unfoldr $ \(a, b) -> let next = b + b - a
                                         in guard (inMap b) $> (b, (b, next))

findAntinodes :: AntennaRule -> Map Point Airspace -> Maybe (Set Point)
findAntinodes getAntinodes mp = do
    keys <- NES.nonEmptySet . M.keysSet $ mp
    let antennas = M.filter (/= Air) mp
    let an = concatMap (\((a, _), (b, _)) -> getAntinodes (inBoundingBox $ boundingBox keys) (a, b))
           . filter (\((p1, n1), (p2, n2)) -> p1 /= p2 && n1 == n2)
           $ (,) <$> M.toList antennas <*> M.toList antennas
    return $ S.fromList an

day08 :: AntennaRule -> Map Point Airspace :~> Int
day08 alg = MkSol
    { sParse = Just . parseAsciiMap parse
    , sShow  = show
    , sSolve = fmap S.size . findAntinodes alg
    }

day08a :: Map Point Airspace :~> Int
day08a = day08 brokenRule

day08b :: Map Point Airspace :~> Int
day08b = day08 correctRule

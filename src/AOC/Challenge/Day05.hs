-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day05 (
    day05a
  , day05b
  ) where

import           AOC.Solver ((:~>)(..))
import           AOC.Common

import Data.Foldable (foldl')
import Control.Monad ((<=<))
import Data.List.Split (splitOn)
import Control.Arrow (first)
import Text.Read (readMaybe)
import Control.Lens (minimumOf)
import Data.IntervalMap.Strict (IntervalMap)
import Data.Interval ((<=..<=))
import Data.Interval (Interval)
import qualified Data.Interval as IV
import qualified Data.IntervalMap.Strict as IVMap
import qualified Data.IntervalSet as IVS

f :: a -> IV.Extended a
f = IV.Finite

uf :: IV.Extended a -> Maybe a
uf (IV.Finite a) = Just a
uf _ = Nothing

parseData :: [String] -> Maybe ([Int], [IntervalMap Int Int])
parseData [] = Nothing
parseData (s:mps) = sequenceTuple (seeds, mps')
    where
        seeds = traverse (readMaybe @ Int) . tail . words $ s
        mkRange (dst, src, rng) = (f src <=..<= f (src + rng), dst - src)
        mps' = traverse (fmap IVMap.fromList . traverse (fmap mkRange . listTup3 <=< traverse (readMaybe @ Int) . words) . tail . lines)
             $ mps

getNext :: IntervalMap Int Int -> Int -> Int
getNext mp = maybe <$> id <*> (+) <*> (`IVMap.lookup` mp)

getNexts :: IVS.IntervalSet Int -> IntervalMap Int Int -> IVS.IntervalSet Int
getNexts xs mp = go <> (IVMap.keysSet $ tmp IVMap.\\ mp)
    where
        tmp = IVMap.fromList . map (,()) . IVS.toList $ xs
        go = IVS.fromList . map ((\(i,d) -> IV.mapMonotonic (+d) i)) . IVMap.toList $ IVMap.intersectionWith const mp tmp

pairs :: [Int] -> [Interval Int]
pairs (x:y:rest) = (f x <=..<= f (x + y)) : pairs rest
pairs _ = []

day05a :: ([Int], [IntervalMap Int Int]) :~> _
day05a = MkSol
    { sParse = parseData . splitOn "\n\n"
    , sShow  = show
    , sSolve = \(s, mps) -> minimumOf traverse . map (\x -> foldl' (flip getNext) x mps) $ s
    }

day05b :: (IVS.IntervalSet Int, [IntervalMap Int Int]) :~> _
day05b = MkSol
    { sParse = fmap (first (IVS.fromList . pairs)) . parseData . splitOn "\n\n"
    , sShow  = show
    , sSolve = \(s, mp) -> uf . IV.lowerBound . IVS.span . foldl' getNexts s $ mp
    }

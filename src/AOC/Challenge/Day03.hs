-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import           AOC.Common  (Point, allNeighbours)
import           AOC.Solver  ((:~>) (..))
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Maybe  (mapMaybe)
import           Linear      (V2 (..))
import           Text.Read   (readMaybe)

i, j :: (RealFrac a, Floating a, Integral b) => a -> b
i = pos sin
j = pos cos

pos :: forall a b. (Integral b, RealFrac a, Floating a) => (a -> a) -> a -> b
pos f n = round . f $ (pi/2) * (fromIntegral . flooredSqrt) (4*n-3)
    where
        flooredSqrt :: a -> b
        flooredSqrt = floor . sqrt

solve :: Double -> Double
solve x = go (IM.singleton 0 (V2 0 0)) (M.singleton (V2 0 0) 1) 1
    where
        go :: IntMap Point -> Map Point Double -> Double -> Double
        go positions soFar n = if s > x then s else go (IM.insert ni p positions) (M.insert p s soFar) (n+1)
            where
                ni = truncate n
                s = sum . mapMaybe (`M.lookup` soFar) . allNeighbours $ p
                V2 i0 j0 = fromIntegral <$> (positions IM.! (ni-1))
                p = V2 (i0 + i n) (j0 + j n)

day03a :: Double :~> Int
day03a = MkSol
    { sParse = readMaybe
    , sShow  = show
    , sSolve = \x -> (Just . sum . abs) (V2 (go i x) (go j x) :: Point)
    }
    where
        go f x = sum [f n | n <- [1..x-1]]

day03b :: Double :~> Int
day03b = MkSol
    { sParse = readMaybe
    , sShow  = show
    , sSolve = Just . truncate . solve
    }

-- |
-- Module      : AOC.Challenge.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.

module AOC.Challenge.Day13 (
    day13a
  , day13b
  ) where

import           AOC.Common (Point, aStar', freqs, manhattan, neighbours)
import           AOC.Solver ((:~>) (..), dyno_)
import           Data.Bits
import           Data.Set   (Set)
import qualified Data.Set   as S
import           Linear
import           Text.Read  (readMaybe)

solve :: Point -> Int -> Maybe (Int, [Point])
solve goal n = aStar' freeNeighbours (manhattan goal) (==goal) (V2 1 1)
    where
        freeNeighbours = freqs . filter (\p -> all (>=0) p && isOpen p) . neighbours
        magic (V2 x y) = (x*x + 3*x + 2*x*y + y + y*y) + n
        isOpen = even . popCount . magic

solveb :: Int -> Int -> Set Point
solveb mx n = S.unions . take (mx + 1) . iterate go $ S.singleton (V2 1 1)
    where
        go = S.unions . S.map freeNeighbours
        freeNeighbours = S.fromList . filter (\p -> all (>=0) p && isOpen p) . neighbours
        magic (V2 x y) = (x*x + 3*x + 2*x*y + y + y*y) + n
        isOpen = even . popCount . magic

day13a :: _ :~> _
day13a = MkSol
    { sParse = readMaybe @ Int
    , sShow  = show
    , sSolve = fmap fst . solve (V2 (dyno_ "x" 31) (dyno_ "y" 39))
    }

day13b :: _ :~> _
day13b = MkSol
    { sParse = readMaybe @ Int
    , sShow  = show
    , sSolve = Just . S.size . solveb (dyno_ "steps" 50)
    }

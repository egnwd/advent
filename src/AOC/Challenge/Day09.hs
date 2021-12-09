-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.

module AOC.Challenge.Day09 (
    day09a
  , day09b
  ) where

import           AOC.Solver            ((:~>)(..))
import           AOC.Common            (parseAsciiMap, neighbours, Point)
import           Data.Char             (digitToInt)
import           Data.Functor.Foldable (apo, ListF(..))
import           Data.List             (sortOn)
import           Data.Monoid           (Sum(Sum), getSum)
import           Data.Maybe            (fromMaybe)
import           Control.Monad         (mfilter)
import qualified Data.Map as M
import qualified Data.Set as S

type Landscape = M.Map Point Int

parse :: String -> Landscape
parse = parseAsciiMap (mfilter (<9) . pure . digitToInt)

score :: Int -> Sum Int
score n = Sum (1+n)

solvea :: Landscape -> Int
solvea = getSum . M.foldMapWithKey (const score) . findLowPoints

findLowPoints :: Landscape -> Landscape
findLowPoints nss = M.filterWithKey isLowest nss
    where
        isLowest k h = all (maybe True (h <) . (`M.lookup` nss)) (neighbours k)

solveb :: Landscape -> Int
solveb land = product . largest 3 . map getBasinSize $ lowPointSeeds
    where
        largest n = take n . sortOn negate
        getBasinSize = sum . apo (buildBasin allNeighbours land)
        allNeighbours = M.mapWithKey (const . S.fromList . neighbours) land
        lowPointSeeds = map (\l -> (S.empty, S.singleton l)) . M.keys . findLowPoints $ land

buildBasin
    :: M.Map Point (S.Set Point)                           -- | Map from point to neighbouring values
    -> Landscape                                           -- | Original grid
    -> (S.Set Point, S.Set Point)                          -- | Seen points, and next points
    -> ListF Int (Either [Int] (S.Set Point, S.Set Point)) -- | ListF of current region sizes over the next regions to visit
buildBasin ns land (seen, next) = Cons (S.size next) go
    where
        neighbouringBasinLocations = M.keysSet . M.restrictKeys land . S.unions . S.map (fromMaybe S.empty . (`M.lookup` ns))
        next' = neighbouringBasinLocations next `S.difference` seen
        seen' = S.union seen next
        go = if null next'
                then Left []
                else Right (seen', next')

day09a :: Landscape :~> Int
day09a = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = Just . solvea
    }

day09b :: Landscape :~> Int
day09b = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = Just . solveb
    }

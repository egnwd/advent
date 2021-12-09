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
import           AOC.Common            (parseAsciiMap, boundingBox, neighbours, Point)
import           Data.Char             (digitToInt)
import           Data.Foldable         (fold)
import           Data.Functor.Foldable (apo, ListF(..))
import           Data.List             (sortOn)
import           Data.Monoid           (Sum(Sum), getSum)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S

type Landscape = M.Map Point Int

parse :: String -> Landscape
parse = parseAsciiMap (pure . digitToInt)

score :: Int -> Sum Int
score n = Sum (1+n)

solvea :: Landscape -> Int
solvea = getSum . M.foldMapWithKey (const score) . findLowPoints

findLowPoints :: Landscape -> Landscape
findLowPoints nss = M.filterWithKey p nss
    where
        bounds = boundingBox . NE.fromList . M.keys $ nss
        p k h = all ((h<) . (nss M.!)) (neighbours bounds k)

solveb :: Landscape -> Int
solveb land = product . largest 3 $ basins
    where
        largest n = take n . sortOn negate
        basins = map snd . M.toList . M.mapWithKey (const . getBasinSize) $ lowPointSeeds
        getBasinSize = getSum . fold . apo (buildBasin allNeighbours land)
        allNeighbours = M.mapWithKey (const . S.fromList . neighbours bounds) land
        bounds = boundingBox . NE.fromList . M.keys $ land
        lowPointSeeds = M.mapKeys (\l -> (S.empty, S.singleton l)) . findLowPoints $ land

buildBasin
    :: M.Map Point (S.Set Point)                                    -- | Map from point to neighbouring values
    -> Landscape                                                    -- | Original grid
    -> (S.Set Point, S.Set Point)                                   -- | Seen points, and next points
    -> ListF (Sum Int) (Either [Sum Int] (S.Set Point, S.Set Point))
buildBasin nss land (seen, next) = Cons (Sum (S.size next)) go
    where
        neighbouringBasinLocations = M.keysSet . M.filter (/=9) . M.restrictKeys land . S.unions . S.map (nss M.!)
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

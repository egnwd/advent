-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day11 (
    day11a
  , day11b
  ) where


import AOC.Solver ((:~>)(..), dyno_)
import AOC.Common.Point (Point, manhattan, parseAsciiSet, boundingBox)
import Control.Lens
import Data.Foldable (foldr')
import Data.List (tails)
import Data.Set (Set, mapMonotonic)
import Linear (V2(..), _x, _y)

import qualified Data.Set          as S
import qualified Data.Set.NonEmpty as NES

solve :: Int -> Set Point -> Int
solve d gs = sum $ zipWith ((sum .) . map . manhattan) newGalaxies (tail . tails $ newGalaxies)
    where
        Just (V2 (V2 xmn ymn) (V2 xmx ymx)) = boundingBox <$> NES.nonEmptySet gs
        newGalaxies = S.toList $ foldr' (mapMonotonic . (shift _y)) (foldr' (S.mapMonotonic . (shift _x)) gs exs) eys
        shift :: Lens' Point Int -> Int -> Point -> Point
        shift dir lim g
          | g ^. dir > lim = g & dir %~ (+(d-1))
          | otherwise = g
        xs = S.map (view _x) gs
        ys = S.map (view _y) gs
        extraColumns = [V2 0 y | y <- [ymn .. ymx], y `S.notMember` ys]
        extraRows = [V2 x 0 | x <- [xmn .. xmx], x `S.notMember` xs]
        V2 exs eys = fmap (filter (/= 0)) . sequence $ extraRows ++ extraColumns

day11a :: Set Point :~> Int
day11a = MkSol
    { sParse = Just . parseAsciiSet (=='#')
    , sShow  = show
    , sSolve = Just . solve 2
    }

day11b :: Set Point :~> Int
day11b = MkSol
    { sParse = Just . parseAsciiSet (=='#')
    , sShow  = show
    , sSolve = Just . solve (dyno_ "d" 1000000)
    }

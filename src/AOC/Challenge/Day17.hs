{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.

module AOC.Challenge.Day17 (
    day17a
  , day17b
  ) where

import AOC.Solver     ((:~>)(..))
import AOC.Common     (parseMaybeLenient, CharParser, pDecimal, Point, inBoundingBox, pastBoundingBox)
import Data.Semigroup (Max(Max), getMax)
import Data.Foldable  (fold)
import Data.Maybe     (mapMaybe)
import Data.List      (find)
import Control.Lens   ((%~), view)
import Linear         (V2(..), _x, _y)

type Velocity = V2 Int
type Region = V2 Point

parser :: CharParser Region
parser = sequence <$> (V2 <$> ("target area: " *> "x=" *> parseRange <* ", ") <*> ("y=" *> parseRange))
    where
        parseRange = V2 <$> pDecimal <*> (".." *> pDecimal)

validRange :: Region -> [Velocity]
validRange (V2 (V2 _ mny) (V2 mxx _)) = sequence $ V2 [1..mxx] [mny..(-mny)]

highestY :: Region -> Int
highestY targ = getMax . fold . mapMaybe ((\t -> findMaxHeight t <$ findCollision targ t) . trajectory targ) $ validRange targ
    where
        findMaxHeight = foldMap (Max . view _y)

numberVelocities :: Region -> Int
numberVelocities targ = length . mapMaybe (findCollision targ . trajectory targ) $ validRange targ

findCollision :: Region -> [Point] -> Maybe Point
findCollision targ = find (inBoundingBox targ)

trajectory :: Region -> Velocity -> [Point]
trajectory targ = takeWhile (not . pastBoundingBox targ) . positions origin

origin :: Point
origin = V2 0 0

positions :: Point -> Velocity -> [Point]
positions p0 = scanl (+) p0 . velocities

velocities :: Velocity -> [Point]
velocities = iterate ((_x %~ dragX) . (_y %~ dragY))
    where
        dragX vx = abs vx - signum vx
        dragY vy = vy - 1

day17a :: Region :~> Int
day17a = MkSol
    { sParse = parseMaybeLenient parser
    , sShow  = show
    , sSolve = Just . highestY
    }

day17b :: Region :~> Int
day17b = MkSol
    { sParse = parseMaybeLenient parser
    , sShow  = show
    , sSolve = Just . numberVelocities
    }

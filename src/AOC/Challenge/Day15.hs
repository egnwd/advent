-- |
-- Module      : AOC.Challenge.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.

module AOC.Challenge.Day15 (
    day15a
  , day15b
  ) where

import           AOC.Solver ((:~>)(..))
import           AOC.Common (parseAsciiMap, aStar, neighbours, boundingBox, Point, manhattan)
import           Data.Char  (digitToInt)
import           Linear
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE

type Cave = M.Map Point Int

findPath :: Cave -> Maybe Int
findPath m = fst <$> go
    where
        (mx, _) = M.findMax m
        ns = M.mapWithKey (\k _ -> (M.restrictKeys m . S.fromList . neighbours) k) m
        go = aStar ns (manhattan mx) (== mx) (V2 0 0)

extendMap :: Cave -> Cave
extendMap m = M.unions tiles
    where
        V2 _ mx = boundingBox . NE.fromList . M.keys $ m
        offset = mx + pure 1
        tiles = map (`moveMap` m) . sequence $ pure [0..4]
        moveMap v@(V2 x y) = M.mapKeys (+(offset*v)) . fmap (\n -> (n-1+x+y) `mod` 9 + 1)

day15 :: (Cave -> Cave) -> Cave :~> Int
day15 f = MkSol
    { sParse = Just . f . parseAsciiMap (pure . digitToInt)
    , sShow  = show
    , sSolve = findPath
    }

day15a :: Cave :~> Int
day15a = day15 id

day15b :: Cave :~> Int
day15b = day15 extendMap

-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.

module AOC.Challenge.Day11 (
    day11a
  , day11b
  ) where

import AOC.Solver ((:~>)(..), dyno_)
import AOC.Common (Point, (!?), lookupFreq, freqs, countTrue, fixedPoint, parseAsciiMap, allNeighbours)
import Data.Char (digitToInt)
import Control.Arrow ((&&&))
import Control.Lens (preview, _head, _1)
import qualified Data.Map as M

type Octopuses = M.Map Point Int

solvea :: Int -> Octopuses -> Maybe Int
solvea n = (!? n) . scanl1 (+) . map (countTrue (==0)) . iterate step

step :: Octopuses -> Octopuses
step = fmap reset . snd . fixedPoint (uncurry next) . ((1 <$) &&& id)
    where
        reset o = if o > 9 then 0 else o
        next :: M.Map Point Int -> Octopuses -> (M.Map Point Int, Octopuses)
        next keys os = (keys', fst <$> os')
            where
                keys' = (`M.restrictKeys` M.keysSet didn'tFlash) . freqs . concatMap allNeighbours . M.keys $ flashed
                (flashed, didn'tFlash) = M.partition snd os'
                os' = M.mapWithKey (boost keys) os

boost :: Octopuses -> Point -> Int -> (Int, Bool)
boost ks k o | k `M.member` ks && o < 10 = let o' = o + lookupFreq k ks in (o', o' > 9)
             | otherwise   = (o, False)

solveb :: Octopuses -> Maybe Int
solveb = preview (_head._1) . dropWhile (not . all (==0) . snd) . zip [0..] . iterate step

day11a :: Octopuses :~> Int
day11a = MkSol
    { sParse = Just . parseAsciiMap (pure . digitToInt)
    , sShow  = show
    , sSolve = solvea (dyno_ "days" 100)
    }

day11b :: Octopuses :~> Int
day11b = MkSol
    { sParse = Just . parseAsciiMap (pure . digitToInt)
    , sShow  = show
    , sSolve = solveb
    }

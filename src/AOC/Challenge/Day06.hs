{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.

module AOC.Challenge.Day06 (
    day06a
  , day06b
  ) where

import AOC.Solver                ((:~>)(..), dyno_)
import AOC.Common                (freqs, pDecimal, parseMaybeLenient, CharParser, (!?))
import Data.Finite               (Finite, finite, unshift, weaken)
import Control.Monad.Combinators (sepBy)
import Data.Map                  (Map)
import qualified Data.Map as M

type Fish = Finite 9

parser :: CharParser [Fish]
parser = (finite <$> pDecimal) `sepBy` ","

solve :: Int -> [Fish] -> Maybe Int
solve n = fmap sum . (!? n) . iterate step . freqs

step :: Map Fish Int -> Map Fish Int
step m = M.insertWith (+) resetFish newFish . M.mapKeys tickDown $ m
    where
        newFish = M.findWithDefault 0 reproducingFish m
        tickDown = maybe babyFish weaken . unshift

resetFish, babyFish, reproducingFish :: Fish
resetFish       = finite 6
babyFish        = finite 8
reproducingFish = finite 0

day06 :: Int -> [Fish] :~> Int
day06 n = MkSol
    { sParse = parseMaybeLenient parser
    , sShow  = show
    , sSolve = solve (dyno_ "days" n)
    }

day06a :: [Fish] :~> Int
day06a = day06 80

day06b :: [Fish] :~> Int
day06b = day06 256

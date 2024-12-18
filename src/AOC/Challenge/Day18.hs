{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : AOC.Challenge.Day18
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 18.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day18 (
    day18a
  , day18b
  ) where

import           AOC.Solver ((:~>)(..), dyno_)
import           AOC.Common
  ( parseLines
  , pDecimal
  , Point
  , aStar'
  , binarySearch
  , neighboursSet
  , inBoundingBox
  , manhattan
  , (!?)
  , sequenceSepBy)

import qualified Data.Map                       as M
import           Data.Set                       (Set)
import qualified Data.Set                       as S
import           Linear (V2(..))
import           Data.Maybe                     (isNothing)

search :: Int -> Set Point -> Maybe (Int, [Point])
search mx mp = aStar' next cost (== V2 mx mx) (V2 0 0)
    where
        next a = M.fromSet (const 1)
               . S.filter (inBoundingBox (V2 (V2 0 0) (V2 mx mx)))
               $ neighboursSet a `S.difference` mp
        cost = manhattan (V2 mx mx)

solve :: Int -> [Point] -> _
solve mx ms = ms !? (binarySearch 0 (length ms) canComplete - 1)
    where
        canComplete n = isNothing $ search mx (S.fromList $ take n ms)

day18a :: [Point] :~> Int
day18a = MkSol
    { sParse = parseLines (V2 pDecimal pDecimal `sequenceSepBy` ",")
    , sShow  = show
    , sSolve = fmap fst . search (dyno_ "mx" 70) . S.fromList . take (dyno_ "bytes" 1024)
    }

day18b :: [Point] :~> Point
day18b = MkSol
    { sParse = parseLines (V2 pDecimal  pDecimal `sequenceSepBy` ",")
    , sShow  = \(V2 x y) -> show x ++ "," ++ show y
    , sSolve = solve (dyno_ "mx" 70)
    }

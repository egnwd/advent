-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.

module AOC.Challenge.Day10
  ( day10a,
    day10b,
  )
where

import AOC.Solver ((:~>)(..), dyno_)
import AOC.Util (strip)
import Control.Lens

step :: String -> String
step [] = []
step w@(x:_) = let (chunk, rest) = span (==x) w in (show . length $ chunk) ++ [x] ++ step rest

day10a :: String :~> Int
day10a =
  MkSol
    { sParse = Just . strip
    , sShow  = show
    , sSolve = fmap length . preview (ix (dyno_ "iterations" 40)) . iterate step
    }

day10b :: String :~> Int
day10b =
  MkSol
    { sParse = Just . strip
    , sShow  = show
    , sSolve = fmap length . preview (ix (dyno_ "iterations" 50)) . iterate step
    }

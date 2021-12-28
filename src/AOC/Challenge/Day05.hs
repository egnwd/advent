{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.

module AOC.Challenge.Day05 (
    day05a
  , day05b
  ) where

import AOC.Solver ((:~>)(..))
import AOC.Common (parseLines, pDecimal, pTok, freqs, lineTo, Point, CharParser)
import Linear
import qualified Data.Map as M

parser :: CharParser (V2 Point)
parser = V2 <$> coord <* pTok "->" <*> coord
    where
        coord = pTok (V2 <$> pDecimal <* "," <*> pDecimal)

solve :: [V2 Point] -> Int
solve = M.size . M.filter (>1) . freqs . concatMap lineTo

isHV :: (Foldable t, Applicative t, Eq a) => V2 (t a) -> Bool
isHV (V2 start end) = or $ (==) <$> start <*> end

day05a :: [V2 Point] :~> Int
day05a = MkSol
    { sParse = parseLines parser
    , sShow  = show
    , sSolve = Just . solve . filter isHV
    }

day05b :: [V2 Point] :~> Int
day05b = MkSol
    { sParse = parseLines parser
    , sShow  = show
    , sSolve = Just . solve
    }

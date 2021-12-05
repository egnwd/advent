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
parser = do
    (x1,y1) <- pTok ((,) <$> pDecimal <* "," <*> pDecimal)
    pTok "->"
    (x2,y2) <- pTok ((,) <$> pDecimal <* "," <*> pDecimal)
    pure $ V2 (V2 x1 y1) (V2 x2 y2)

solve :: [V2 Point] -> Int
solve = M.size . M.filter (>1) . freqs . concatMap lineTo

isHV :: V2 Point -> Bool
isHV (V2 (V2 x1 y1) (V2 x2 y2)) = x1 == x2 || y1 == y2

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

{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day05 (
    day05a
  , day05b
  ) where

import           AOC.Common (CharParser, pDecimal, countTrue, parseMaybeLenient)
import           AOC.Solver ((:~>)(..))

import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import Data.Interval ((<=..<=), Interval, width)
import           Data.IntervalSet               (IntervalSet)
import qualified Data.IntervalSet               as ISet

parser :: CharParser (IntervalSet Int, [Int])
parser = do
    fresh <- range `P.sepEndBy` P.newline
    P.newline
    ingredients <- pDecimal `P.sepBy` P.newline
    return (ISet.fromList fresh, ingredients)
    where
        range :: CharParser (Interval Int)
        range = do
            l <- pDecimal <* "-"
            h <- pDecimal
            return (l <=..<= h)

day05a :: (IntervalSet Int, [Int]) :~> Int
day05a = MkSol
    { sParse = parseMaybeLenient parser
    , sShow  = show
    , sSolve = \(f,i) -> Just $ countTrue (`ISet.member` f) i
    }

day05b :: (IntervalSet Int, [Int]) :~> Int
day05b = MkSol
    { sParse = parseMaybeLenient parser
    , sShow  = show
    , sSolve = Just . sum . map (succ . width) . ISet.toList . fst
    }

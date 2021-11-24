{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.

module AOC.Challenge.Day12 (
    day12a
  , day12b
  ) where

import AOC.Solver ((:~>)(..))
import AOC.Common (CharParser, parseMaybeLenient, pDecimal)
import Text.Megaparsec (many, takeRest, takeWhileP, try)
import Data.Aeson (Value(..), decode)
import Data.Scientific (floatingOrInteger)
import Data.ByteString.Lazy.UTF8 as BLU (fromString)
import Data.Either (fromRight)
import Data.Char (isDigit)

parseNumbers :: CharParser [Integer]
parseNumbers = many (try parseNumber) <* takeRest
    where
        parseNumber :: CharParser Integer
        parseNumber = takeWhileP Nothing (\c -> not $ c == '-' || isDigit c) *> pDecimal

walkNonRed :: Value -> [Int]
walkNonRed (Number n) = [fromRight 0 (floatingOrInteger n :: Either Double Int)]
walkNonRed (Array a) = foldMap walkNonRed a
walkNonRed (Object o)
    | foldr hasRed False o = []
    | otherwise = foldMap walkNonRed o
walkNonRed _ = []

hasRed :: Value -> Bool -> Bool
hasRed (String "red") = const True
hasRed _ = id

day12a :: _ :~> _
day12a = MkSol
    { sParse = parseMaybeLenient parseNumbers
    , sShow  = show
    , sSolve = Just . sum
    }

day12b :: Value :~> _
day12b = MkSol
    { sParse = decode . BLU.fromString
    , sShow  = show
    , sSolve = Just . sum . walkNonRed
    }

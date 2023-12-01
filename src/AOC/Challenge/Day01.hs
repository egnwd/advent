{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day01 (
    day01a
  , day01b
  ) where


import AOC.Solver    ((:~>)(..))
import Control.Monad (guard)
import Data.Char     (isNumber, digitToInt)
import Data.Functor  (($>))
import Data.List     (tails, isPrefixOf)
import Data.Maybe    (mapMaybe)

topAndTail :: [Int] -> Int
topAndTail xs = head xs * 10 + last xs

pattern One, Two, Three, Four, Five, Six, Seven, Eight, Nine :: String
pattern One   <- (isPrefixOf "one"   -> True)
pattern Two   <- (isPrefixOf "two"   -> True)
pattern Three <- (isPrefixOf "three" -> True)
pattern Four  <- (isPrefixOf "four"  -> True)
pattern Five  <- (isPrefixOf "five"  -> True)
pattern Six   <- (isPrefixOf "six"   -> True)
pattern Seven <- (isPrefixOf "seven" -> True)
pattern Eight <- (isPrefixOf "eight" -> True)
pattern Nine  <- (isPrefixOf "nine"  -> True)

numberMap :: String -> Maybe Int
numberMap One     = Just 1
numberMap Two     = Just 2
numberMap Three   = Just 3
numberMap Four    = Just 4
numberMap Five    = Just 5
numberMap Six     = Just 6
numberMap Seven   = Just 7
numberMap Eight   = Just 8
numberMap Nine    = Just 9
numberMap (x : _) = guard (isNumber x) $> digitToInt x
numberMap []      = Nothing

day01a :: [String] :~> Int
day01a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = Just . sum . map (topAndTail . map digitToInt . filter isNumber)
    }

day01b :: [String] :~> Int
day01b = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = Just . sum . map (topAndTail . mapMaybe numberMap . tails)
    }

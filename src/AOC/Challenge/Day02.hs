{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import           AOC.Solver           ((:~>)(..))
import           AOC.Common           (CharParser, parseLines, parseAsciiMap, dirVec', Dir(..), Point)
import           Control.Monad        (when, guard)
import           Control.Monad.State  (get, gets, put, evalState)
import           Data.Char            (isAlphaNum)
import           Data.Foldable        (fold)
import           Data.Functor         (($>))
import           Data.Map             (Map, (!), member)
import           Linear               (V2(..))
import           Text.Heredoc         (here)
import           Text.Megaparsec      (choice, many)
import           Text.Megaparsec.Char (char)

normalKeypad :: Map Point String
normalKeypad = parseAsciiMap (\x -> guard (isAlphaNum x) $> [x]) . drop 1 $ [here|
123
456
789|]

commiteeKeypad :: Map Point String
commiteeKeypad = parseAsciiMap (\x -> guard (isAlphaNum x) $> [x]) . drop 1 $ [here|
  1
 234
56789
 ABC
  D|]

parser :: CharParser [V2 Int]
parser = many (dirVec' <$> choice [North <$ char 'U', West <$ char 'L', South <$ char 'D', East <$ char 'R'])

solve :: Map Point String -> Point -> [[V2 Int]] -> [String]
solve keypad start iss = evalState (traverse go iss) start
    where
        go [] = gets (keypad !)
        go (i:is) = do
            p <- get
            let p' = p + i
            when (p' `member` keypad) (put p')
            go is

day02 :: Map Point String -> Point -> [[V2 Int]] :~> [String]
day02 keypad start = MkSol
    { sParse = parseLines parser
    , sShow  = fold
    , sSolve = Just . solve keypad start
    }

day02a :: [[V2 Int]] :~> [String]
day02a = day02 normalKeypad (V2 1 1)

day02b :: [[V2 Int]] :~> [String]
day02b = day02 commiteeKeypad (V2 0 2)

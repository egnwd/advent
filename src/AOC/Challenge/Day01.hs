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

import           AOC.Common                (countTrue, CharParser, parseLines, pDecimal)
import           AOC.Solver                ((:~>)(..))
import           Control.Applicative       ((<|>))
import           Data.List                 (mapAccumL)
import qualified Text.Megaparsec.Char as P

parser :: CharParser (Either Int Int)
parser = (Left <$> (P.char 'L' *> pDecimal)) <|> (Right <$> (P.char 'R' *> pDecimal))

rotate :: Int -> Either Int Int -> Int
rotate n (Left x) = (n - x) `mod` 100
rotate n (Right x) = (n + x) `mod` 100

next :: Int -> Either Int Int -> [Either Int Int]
next 0 (Left l) = next 100 (Left l)
next 100 (Right r) = next 0 (Right r)
next initial (Left n)
  | n > initial = Left initial : next 100 (Left $ n - initial)
  | otherwise   = [Left n]
next initial (Right n)
  | n > (100-initial) = Right (100-initial) : next 0 (Right $ n - (100-initial))
  | otherwise         = [Right n]

go :: Int -> [Either Int Int] -> (Int, [[Int]])
go = mapAccumL (\n x -> mapAccumL (\s a -> (rotate s a, s)) n $ next n x)

day01a :: [Either Int Int] :~> Int
day01a = MkSol
    { sParse = parseLines parser
    , sShow  = show
    , sSolve = Just . countTrue (==0) . scanl rotate 50
    }

day01b :: [Either Int Int] :~> Int
day01b = MkSol
    { sParse = parseLines parser
    , sShow  = show
    , sSolve = Just . countTrue (==0) . concat . snd . go 50
    }

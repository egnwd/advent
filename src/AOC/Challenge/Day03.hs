-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import AOC.Solver     ((:~>)(..))
import AOC.Common     (loopEither)
import Data.List      (transpose)
import Data.Char      (digitToInt)
import Data.Bifunctor (first, second)
import Control.Lens   (each, (%~))
import Data.Finite

type Bit = Finite 2
type Bits = [Bit]

zero, one :: Bit
zero = finite 0
one = finite 1

parser :: String -> Maybe [Bits]
parser = traverse (traverse (packFinite . fromIntegral . digitToInt)) . lines

mostAndLeastCommonBit :: Bits -> (Bit, Bit)
mostAndLeastCommonBit bs = let b = mostCommonBit bs in (b, 1-b)

leastCommonBit, mostCommonBit :: Bits -> Bit
leastCommonBit = (1-) . mostCommonBit
mostCommonBit bs = let (zeros, ones) = foldr go (0,0) bs
                    in if ones >= zeros then one else zero
    where
        go :: Bit -> (Int, Int) -> (Int, Int)
        go 0 = first succ
        go 1 = second succ

binToDec :: Bits -> Int
binToDec = foldl (\a -> (+) (2*a) . fromEnum . (== 1)) 0

solvea :: [Bits] -> Int
solvea xs = let (γ, ε) = (each %~ binToDec) . unzip . map mostAndLeastCommonBit . transpose $ xs
             in γ * ε

solveb :: [Bits] -> Int
solveb bs = o * c
    where
        o = binToDec $ loopEither oxygen (0, bs)
        c = binToDec $ loopEither co2 (0, bs)

oxygen, co2 :: (Int, [Bits]) -> Either Bits (Int, [Bits])
oxygen = criteria mostCommonBit
co2 = criteria leastCommonBit

criteria :: _ -> (Int, [Bits]) -> Either Bits (Int, [Bits])
criteria f (i, xs) = case filter ((== b) . (!! i)) xs of
                       [n] -> Left n
                       ns  -> Right (succ i, ns)
   where
       bs = map f . transpose $ xs
       b = bs !! i

day03a :: [Bits] :~> Int
day03a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . solvea
    }

day03b :: [Bits] :~> Int
day03b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . solveb
    }

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

import AOC.Solver            ((:~>)(..))
import Data.List             (transpose)
import Data.Char             (digitToInt)
import Data.Bifunctor        (first, second)
import Data.Finite           (Finite, finite, packFinite)
import Data.Functor.Foldable (ListF(..), apo)
import Control.Arrow         ((&&&))
import Linear.V2             (V2(V2))

type Bit = Finite 2
type Bits = [Bit]

zero, one :: Bit
zero = finite 0
one  = finite 1

parser :: String -> Maybe [Bits]
parser = traverse (traverse (packFinite . fromIntegral . digitToInt)) . lines

mostAndLeastCommonBit :: Bits -> (Bit, Bit)
mostAndLeastCommonBit bs = let b = mostCommonBit bs in (b, 1-b)

leastCommonBit, mostCommonBit :: Bits -> Bit
leastCommonBit = (1-) . mostCommonBit
mostCommonBit bs = if ones >= zeros then one else zero
    where
        (zeros, ones) = foldr go (0,0) bs
        go :: Bit -> (Int, Int) -> (Int, Int)
        go 0 = first succ
        go 1 = second succ
        go _ = id

binToDec :: Bits -> Int
binToDec = foldl (\a -> (+) (2*a) . fromEnum . (== 1)) 0

binaryProduct :: (Bits, Bits) -> Int
binaryProduct = product . fmap binToDec . uncurry V2

solvea :: [Bits] -> Int
solvea = binaryProduct . unzip . map mostAndLeastCommonBit . transpose

solveb :: [Bits] -> Int
solveb = binaryProduct . (apo oxygen &&& apo co2)

oxygen, co2 :: [Bits] -> ListF Bit (Either Bits [Bits])
oxygen = criteria mostCommonBit
co2    = criteria leastCommonBit

criteria :: (Bits -> Bit) -> [Bits] -> ListF Bit (Either Bits [Bits])
criteria f bs = go
    where
        b = (f . map head) bs
        ns = filter ((==b) . head) bs
        go = case ns of
              []  -> Nil
              [n] -> Cons b (Left (tail n))
              _   -> Cons b (Right (map tail ns))

day03a :: [Bits] :~> Int
day03a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . solvea
    }

day03b :: [Bits] :~> _
day03b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . solveb
    }

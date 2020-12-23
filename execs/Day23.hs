{-# LANGUAGE BangPatterns #-}
{-|
   Name: Crab Cups
   Url: <https://adventofcode.com/2020/day/23>
-}

module Day23 (main) where

import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Array.ST
import Data.Array.Unboxed

type Input = [Int]
type Output = Int

main :: IO ()
main = print . (part1 &&& part2) $ parseInput

parseInput = [9,1,6,4,3,8,2,7,5]

mkArray :: [Int] -> ST s (STUArray s Int Int)
mkArray xs = do
  arr <- newArray_ (minimum xs, maximum xs)   -- init array the size of the range of input
  arr <$ zipWithM_ (writeArray arr) xs (drop 1 $ cycle xs) -- cup i (index i) -> cup j (value at i)
{-# INLINE mkArray #-}

move :: (MArray a e m, Enum e, Ix e, Show e) => a e e -> e -> m e
move arr i = do
  -- start: i -> t1 -> t2 -> t3 -> ...
  t1 <- readArray arr i
  t2 <- readArray arr t1
  t3 <- readArray arr t2
  i' <- readArray arr t3

  (mn, mx) <- getBounds arr
  let next n = if pred n < mn then mx else pred n
      j = until (\a -> a /= t1 && a /= t2 && a /= t3) next (next i)
  -- interest: j -> k
  k <- readArray arr j
  -- next: i -> i' -> ... -> j -> t1 -> t2 -> t3 -> k
  writeArray arr t3 k
  writeArray arr j t1
  writeArray arr i i'

  return i'
{-# INLINE move #-}

mkNumber :: UArray Int Int -> (Int, Int) -> (Int, Int)
mkNumber arr (i, acc) = let j = arr ! i in (j, acc * 10 + j)
{-# INLINE mkNumber #-}

part1 :: Input -> Output
part1 input = runST $ do
  cups <- mkArray input
  foldM_ (const . move cups) (head input) [1..100]
  cups' <- freeze cups
  let (mn, mx) = bounds cups'
      (_, n) = foldl' (const . mkNumber cups') (1, 0) [mn .. mx-1]
  return n

part2 :: Input -> Output
part2 input = runST $ do
  !cups <- mkArray (input ++ [10..1000000])
  foldM_ (const . move cups) (head input) [1..10000000]
  a <- readArray cups 1
  b <- readArray cups a
  return (a * b)

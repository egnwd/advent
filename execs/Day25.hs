{-# LANGUAGE BangPatterns #-}
{-|
   Name: Combo Breaker
   Url: <https://adventofcode.com/2020/day/25>
-}

module Day25 (main) where

import Advent

subject = 7
modNumber = 20201227

main :: IO ()
main = print . part1 =<< getParsedLines 25 number

type Input  = [Int]
type Output = Int

part1 :: Input -> Output
part1 [cardPublicKey, doorPublicKey] =
  let cardLoopSize = fst . head . filter ((== cardPublicKey) . snd) . iterate (solve subject) $ (0, subject)
   in snd . (!! cardLoopSize) . iterate (solve doorPublicKey) $ (0, doorPublicKey)

solve subj (!i, !acc) = (i+1, (acc * subj) `rem` modNumber)
{-# INLINE solve #-}

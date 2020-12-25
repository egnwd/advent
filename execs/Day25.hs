{-|
   Name: Combo Breaker
   Url: <https://adventofcode.com/2020/day/25>
-}

module Day25 (main) where

import Advent
import Math.NumberTheory.Powers.Modular

subject = 7
modNumber = 20201227

main :: IO ()
main = print . part1 =<< getParsedLines 25 number

part1 [cardPublicKey, doorPublicKey] =
  let cardLoopSize = fst . head . filter ((== cardPublicKey) . snd) . zip [1..] . iterate ((`rem` modNumber) . (*subject)) $ subject
   in powModInt doorPublicKey cardLoopSize modNumber

{-|
   Name: 
   Url: <https://adventofcode.com/2020/day/X>
-}

module DayXX (main) where

import Advent
import Prelude hiding (unlines)

import qualified Data.Text as T

main :: IO ()
main = do
  input <- getParsedLines X parseInput
  print $ part1 input
  print $ part2 input

type Input  = [T.Text]
type Output = Int

-- | Parsing
parseInput :: Parser T.Text
parseInput = undefined

part1 :: Input -> Output
part1 = const 0

part2 :: Input -> Output
part2 = const 0

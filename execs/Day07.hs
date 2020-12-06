{-|
   Name: 
   Url: <https://adventofcode.com/2020/day/7>
-}

module Day07 (main) where

import Advent
import Prelude hiding (unlines)

import qualified Data.Text as T

main :: IO ()
main = do
  input <- getParsedLines 7 parseInput
  print $ part1 input
  print $ part2 input

type Input  = [T.Text]
type Output = Int

-- | Parsing
parseInput :: Parser T.Text
parseInput = undefined

part1 :: Input -> Output
part1 = undefined

part2 :: Input -> Output
part2 = undefined

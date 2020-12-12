{-|
   Name: 
   Url: <https://adventofcode.com/2020/day/X>
-}

module DayXX (main) where

import Advent
import Prelude hiding (unlines)
import Control.Arrow ((&&&))

import qualified Data.Text as T

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedLines X parseInput

type Input  = [T.Text]
type Output = Int

-- | Parsing
parseInput :: Parser T.Text
parseInput = undefined

part1 :: Input -> Output
part1 = const 0

part2 :: Input -> Output
part2 = const 0

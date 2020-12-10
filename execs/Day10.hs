{-|
   Name: Adapter Array
   Url: <https://adventofcode.com/2020/day/10>
-}

module Day10 (main) where

import Advent
import Prelude hiding (unlines)
import Control.Arrow ((&&&))

import Data.Maybe
import Data.List (sort)
import Data.Map ((!))
import Data.List.Split

import qualified Data.Map as M hiding ((!))

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedLines 10 parseInput

type Input  = [Int]
type Output = Int

-- | Parsing
parseInput :: Parser Int
parseInput = fromIntegral <$> number

part1 :: Input -> Output
part1 input = let diffs = joltDiffs input
                  dist = foldr (M.alter (return . (+1) . fromMaybe 0)) mempty diffs
               in (dist ! 1) * (dist ! 3)

part2 :: Input -> Output
part2 input = let diffs = joltDiffs input
                  oneIslands = filter (not . null) $ splitWhen (==3) diffs
               in product $ map (sum . combinations . pred . length) oneIslands

joltDiffs input = let plugs = sort $ (maximum input + 3) : input
                   in zipWith (-) plugs (0:plugs)

choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k

combinations a = map (choose a . (a-)) [0, 1, 2]


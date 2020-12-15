{-|
   Name: Binary Boarding
   Url: <https://adventofcode.com/2020/day/5>
-}

module Day05 (main) where

import Advent
import Prelude hiding (unlines)

import Control.Arrow ((&&&))
import Data.Semigroup
import Data.List

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.Megaparsec as MP

readBinary :: String -> Int
readBinary = bitsToInt
  where
    bitsToInt = foldl' toBin 0
    bin 'L' = 0
    bin 'R' = 1
    bin 'F' = 0
    bin 'B' = 1
    toBin acc i = 2*acc+bin i

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedLines 5 parseInput

type Input  = [Int]
type Output = Int

letters = S.fromList [ 'L', 'R', 'F', 'B' ]

-- | Parsing
parseInput :: Parser Int
parseInput = readBinary . T.unpack <$> MP.takeWhileP Nothing (`S.member` letters)


part1 :: Input -> Output
part1 = getMax . foldMap Max

part2 :: Input -> Output
part2 input = let (mx, mn, sm) = ap3 (getMax, getMin, getSum) $ foldMap (\x -> (Max x, Min x, Sum x)) input
                  total = (mx*(mx+1)-mn*(mn-1)) `div` 2
               in total - sm

ap3 (f, g, h) (a, b, c) = (f a, g b, h c)

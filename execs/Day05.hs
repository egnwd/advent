{-|
   Name: Binary Boarding
   Url: <https://adventofcode.com/2020/day/5>
-}

module Main
  ( main
  ) where

import Advent
import Prelude hiding (unlines)

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
    toBin acc i = 2*acc+(bin i)

main :: IO ()
main = do
  input <- getParsedLines 5 parseInput
  print $ part1 input
  print $ part2 input

type Input  = [Int]
type Output = Int

letters = S.fromList $ [ 'L', 'R', 'F', 'B' ]

-- | Parsing
parseInput :: Parser Int
parseInput = readBinary . T.unpack <$> MP.takeWhileP Nothing (`S.member` letters)


part1 :: Input -> Output
part1 = getMax . foldMap (Max)

part2 :: Input -> Output
part2 input = let x    = sort $ input
                  diff = zipWith (-) (drop 1 x) x
               in (+1) . snd . head $ filter ((2 ==) . fst) $ zip diff x

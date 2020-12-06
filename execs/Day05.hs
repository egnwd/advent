{-|
   Name: Binary Boarding
   Url: <https://adventofcode.com/2020/day/5>
-}

module Day05 (main) where

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
part2 input = let (mx, mn, sm) = foldl' (\(u,l,s) x -> (max u x, min l x, s+x)) (0, 1000000, 0) input
                  total = (mx*(mx+1)-mn*(mn-1)) `div` 2
               in total - sm

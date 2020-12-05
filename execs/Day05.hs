{-|
   Name: Binary Boarding
   Url: <https://adventofcode.com/2020/day/5>
-}

module Main
  ( main
  ) where

import Advent
import Prelude hiding (unlines)

import Refined
import Data.Finite
import Data.Bits
import Data.Maybe
import Data.Semigroup
import Data.List
import Control.Lens
import Control.Monad

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.Megaparsec as MP

type Binary = Refined (SizeEqualTo 10) [Finite 2]

readBinary :: String -> Int
readBinary = bitsToInt . unrefine . readListBits
  where
    bitsToInt :: [Finite 2] -> Int
    bitsToInt = foldl' toBin 0 . zip [9,8..0]
    readListBits :: String -> Binary
    readListBits = fromJust . (refineFail <=< traverse (preview (to bin)))
    bin :: Char -> Finite 2
    bin 'L' = finite 0
    bin 'R' = finite 1
    bin 'F' = finite 0
    bin 'B' = finite 1
    toBin :: Int -> (Int, Finite 2) -> Int
    toBin acc (_, 0) = acc
    toBin acc (i, 1) = setBit acc i

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

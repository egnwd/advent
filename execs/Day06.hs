{-|
   Name:
   Url:  <https://adventofcode.com/2020/day/6>
-}

module Day06 (main) where

import Advent

import Data.Char
import Text.Megaparsec (sepEndBy, takeWhileP)

import qualified Data.Set as S
import qualified Data.Text as T

main :: IO ()
main = do
  input <- getParsedDoubleLines 6 (parseInput S.unions)
  input2 <- getParsedDoubleLines 6 (parseInput (foldl1 S.intersection))
  print $ part input
  print $ part input2

type Letter = Char

type Input  = [S.Set Letter]
type Output = Int

-- | Parsing
parseInput :: ([S.Set Letter] -> S.Set Letter) -> Parser (S.Set Letter)
parseInput agg = agg <$> parsePerson `sepEndBy` singleSpace

parsePerson :: Parser (S.Set Letter)
parsePerson = S.fromList . T.unpack <$> takeWhileP Nothing isAlphaNum

part :: Input -> Output
part = sum . map S.size

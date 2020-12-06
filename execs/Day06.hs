{-|
   Name: Custom Customs
   Url:  <https://adventofcode.com/2020/day/6>
-}

module Day06 (main) where

import Advent

import Control.Arrow ((***))
import Data.Foldable
import Data.Char
import Text.Megaparsec (sepEndBy, takeWhileP)

import qualified Data.Set as S
import qualified Data.Text as T

main :: IO ()
main = do
  (input, input2) <- unzip <$> getParsedDoubleLines 6 parseInput
  print $ part input
  print $ part input2

type Letter = Char

type Input  = [S.Set Letter]
type Output = Int

allLetters = S.fromList "abcdefghijklmnopqrstuvwxyz"

-- | Parsing
parseInput :: Parser (S.Set Letter, S.Set Letter)
parseInput = foldl' (flip $ \p -> S.union p *** S.intersection p) (mempty, allLetters) <$> parsePerson `sepEndBy` singleSpace

parsePerson :: Parser (S.Set Letter)
parsePerson = S.fromList . T.unpack <$> takeWhileP Nothing isAlphaNum

part :: Input -> Output
part = sum . map S.size

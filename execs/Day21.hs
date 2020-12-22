{-|
   Name: Allergen Assessment
   Url: <https://adventofcode.com/2020/day/21>
-}

module Day21 (main) where

import Advent
import Control.Arrow ((&&&))
import Data.Char
import Data.Monoid
import Data.List
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedLines 21 parseInput

type Input  = [M.Map T.Text (S.Set T.Text)]

part1 :: Input -> Int
part1 input = let possibleIngredients = S.unions . M.elems . M.unionsWith S.intersection $ input
                  allIngredients      = S.unions . M.elems . M.unionsWith S.union        $ input
                  notPossible         = allIngredients S.\\ possibleIngredients
               in getSum . foldMap (Sum . (S.size . (`S.intersection` notPossible)) . S.unions . M.elems) $ input

part2 :: Input -> T.Text
part2 input = let possibilities = sortOn (length . snd) . M.toList . M.unionsWith S.intersection $ input
               in T.intercalate "," . M.elems . fromJust . listToMaybe $ pick possibilities S.empty M.empty

pick [] _ assignments = return assignments
pick ((k, ingr):rest) chosen assignments = do
  i <- S.toList (ingr S.\\ chosen)
  pick rest (S.insert i chosen) (M.insert k i assignments)
--
-- | Parsing
parseInput :: Parser (M.Map T.Text (S.Set T.Text))
parseInput = do
  ingr <- S.fromList <$> word `sepEndBy` hspace
  allergens <- between "(contains " ")" (word `sepBy` symbol ",")
  return $ M.fromList [(k, ingr) | k <- allergens]

word :: Parser T.Text
word = takeWhile1P Nothing isAlpha


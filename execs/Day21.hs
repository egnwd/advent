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
import Debug.Trace

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedLines 21 parseInput

type Input  = [M.Map T.Text (S.Set T.Text)]
type Output = Int

-- | Parsing
parseInput :: Parser (M.Map T.Text (S.Set T.Text))
parseInput = do
  ingr <- S.fromList <$> word `sepEndBy` hspace
  allergens <- between "(contains " ")" (word `sepBy` symbol ",")
  return $ M.fromList [(k, ingr) | k <- allergens]

word :: Parser T.Text
word = takeWhile1P Nothing isAlpha

part1 input = let a = S.unions . M.elems . M.unionsWith S.intersection $ input
                  b = S.unions . M.elems . M.unionsWith S.union $ input
                  np = b S.\\ a
               in getSum . foldMap (Sum . M.foldr (\s acc -> acc + S.size (s `S.intersection` np)) 0 . M.mapKeysWith S.union (const 0)) $ input

part2 input = let a = M.toList . M.unionsWith S.intersection $ input
                  b = sortOn (length . snd) a
               in T.intercalate "," . M.elems . fromJust . listToMaybe $ pick b S.empty M.empty

pick [] _ assignments = return assignments
pick ((k, ingr):rest) chosen assignments = do
  i <- S.toList (ingr S.\\ chosen)
  pick rest (S.insert i chosen) (M.insert k i assignments)

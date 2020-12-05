{-|
   Name: Hill Sleding
   Url: <https://adventofcode.com/2020/day/3>
-}

module Day03 where

import Advent
import Prelude hiding (unlines)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Control.Monad.State
import Control.Lens

import Data.Vector (Vector(..), (!), fromList)

-- $setup
-- >>> let parse = Data.Either.fromRight undefined . Advent.parseLines parseInput
-- >>> :set -XOverloadedStrings

main :: IO ()
main = do
  input <- getParsedLines 3 parseInput
  print $ sled (1,3) input
  print $ product (map (flip sled input) [(1,1), (1,3), (1,5), (1,7), (2,1)])

data Terrain = Open | Tree deriving Show
type Input   = [Vector Terrain]
type Output  = Int

-- | Parsing
parseInput :: Parser (Vector Terrain)
parseInput = fromList <$> many parseTerrain

parseTerrain :: Parser Terrain
parseTerrain = Open <$ char '.' <|> Tree <$ char '#'

-- | Sled down the hill counting the number of trees encountered
--
-- >>> let hill = Data.Text.unlines $ [ "..##.......", "#...#...#..", ".#....#..#.", "..#.#...#.#", ".#...##..#.", "..#.##.....", ".#.#.#....#", ".#........#", "#.##...#...", "#...##....#", ".#..#...#.#"]
-- >>> let input = parse hill
-- >>> sled (1,3) input
-- 7
--
-- >>> product (map (flip sled input) [(1,1), (1,3), (1,5), (1,7), (2,1)])
-- 336
sled :: (Int, Int) -> Input -> Output
sled (d,a) hill = evalState (go a (takeEvery0 d hill)) (0, 0)

go :: Int -> Input -> State (Int, Output) Output
go _ []     = gets snd
go a (r:rs) = do
  let w = length r
  c <- _1 <<%= nextCoord a w
  case r ! c of
    Open -> go a rs
    Tree -> _2 += 1 >> go a rs

takeEvery0 _ [] = []
takeEvery0 n (x:xs) = x : takeEvery n xs
takeEvery n xs =
  case drop (n-1) xs of
    (y:ys) -> y : takeEvery n ys
    [] -> []

-- | Moves 3 along and wraps
--
-- >>> nextCoord 3 11 0
-- 3
--
-- >>> nextCoord 3 11 9
-- 1
--
nextCoord s w c = (c + s) `mod` w

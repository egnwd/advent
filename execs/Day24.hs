{-|
   Name: Lobby Layout
   Url: <https://adventofcode.com/2020/day/24>
-}

module Day24 (main) where

import Advent
import Advent.Hex
import Control.Arrow ((&&&))
import Text.Megaparsec
import Control.Monad
import Data.Foldable
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedLines 24 parseInput

type Floor  = S.Set Point
type Tile   = [Point]
data Colour = White | Black deriving (Show, Eq, Ord)

type Input  = [Tile]
type Output = Int

parseInput :: Parser Tile
parseInput = map toVec <$> some parseCardinality

part1 :: Input -> Output
part1 = length . filter (==Black) . M.elems . followPaths

part2 :: Input -> Output
part2 = S.size . (!! 100) . iterate newDay . M.keysSet . M.filter (== Black) . followPaths

followPaths = foldl' (flip followPath) M.empty
followPath ts = M.alter changeFloor (sum ts)
changeFloor = pure . maybe Black flipTile

flipTile White = Black
flipTile Black = White

newDay :: Floor -> Floor
newDay floor =
  M.keysSet . M.filterWithKey (rule . (`S.member` floor))   -- Filter to those that are black for the next day of the exhibit
    . M.unionsWith (+)                                      -- Group and count number of times that neighbour is "seen"
    . S.map (M.fromSet (const 1) . S.fromList . neighbours) -- Get the neighbours of each tile
    $ floor

rule black n = n == 2 || (black && n == 1)

neighbours ix = do
  d <- S.toList allDirections
  guard $ d /= pure 0
  return $ ix + d

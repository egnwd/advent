{-|
   Name: Conway Cubes
   Url: <https://adventofcode.com/2020/day/17>
-}

module Day17 (main) where

import Advent
import Control.Arrow ((&&&))
import Control.Monad
import Linear
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Set as S
import qualified Data.Map as M

main :: IO ()
main = print . (part mkCube &&& part mkTessaract) =<< getParsedInput 17 parseInput

parseInput :: Parser (S.Set (V2 Int))
parseInput =
      S.fromList . map fst . filter snd . concat                            -- Take the alive cells and turn into set
    . zipWith (\i -> zipWith (\j -> (V2 j i,)) [0..]) [0..]                 -- Add an index to all the cells
  <$> ((`sepBy` newline) . many $ (True <$ char '#' <|> False <$ char '.')) -- Parse '#' as Alive and '.' as Not Alive in a grid

part f = length . (!! 6) . iterate step . S.map f

step wld =
    M.keysSet . M.filterWithKey (rule . (`S.member` wld))  -- Filter to those that live to the next generation
  . M.unionsWith (+)                                       -- Group and count number of times that neighbour is "seen"
  . S.map (M.fromSet (const 1) . S.fromList . neighbours)  -- Get the neighbours of each cell
  $ wld

rule True  n = n == 2 || n == 3
rule False n = n == 3

mkCube (V2 x y) = V3 x y 0
mkTessaract = vector . mkCube

neighbours ix = do
  d <- sequence (pure [-1..1])
  guard $ d /= pure 0
  return $ ix + d

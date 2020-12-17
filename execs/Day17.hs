{-|
   Name: Conway Cubes
   Url: <https://adventofcode.com/2020/day/17>
-}

module Day17 (main) where

import Advent
import Control.Arrow ((&&&))
import Control.Monad
import Linear
import Data.List
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = print . (part mkCube &&& part mkTessaract) =<< getParsedInput 17 parseInput

parseInput :: Parser [V2 Int]
parseInput = map fst . filter snd . concat . zipWith (\i xs -> zipWith (\j x -> (V2 j i, x)) [0..] xs) [0..]
  <$> ((`sepBy` newline) . many $ (True <$ char '#' <|> False <$ char '.'))

part f = length . (!! 6) . iterate step . map f

step wld = do
  (c, n) <- numNeighbours $ concatMap neighbours wld
  guard $ rule (c `elem` wld) n
  return c

rule True  n = n == 2 || n == 3
rule False n = n == 3

numNeighbours :: Ord a => [a] -> [(a, Int)]
numNeighbours xs = do
  x <- group $ sort xs
  return (head x, length x)

mkCube (V2 x y) = V3 x y 0
mkTessaract = vector . mkCube

neighbours ix = do
  d <- sequence (pure [-1..1])
  guard $ d /= pure 0
  return $ ix + d

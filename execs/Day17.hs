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
parseInput = S.fromList . map fst . filter snd . concat . zipWith (\i xs -> zipWith (\j x -> (V2 j i, x)) [0..] xs) [0..]
  <$> ((`sepBy` newline) . many $ (True <$ char '#' <|> False <$ char '.'))

part f = length . (!! 6) . iterate step . S.map f

step wld =
    M.keysSet . M.filterWithKey (rule . (`S.member` wld))
  . M.unionsWith (+)
  . S.map (M.fromSet (const 1) . S.fromList . neighbours)
  $ wld

rule True  n = n == 2 || n == 3
rule False n = n == 3

mkCube (V2 x y) = V3 x y 0
mkTessaract = vector . mkCube

neighbours ix = do
  d <- sequence (pure [-1..1])
  guard $ d /= pure 0
  return $ ix + d

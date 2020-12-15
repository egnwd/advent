{-|
   Name: Encoding Error
   Url: <https://adventofcode.com/2020/day/9>
-}

module Day09 (main) where

import Advent
import Prelude hiding (unlines)
import Data.List
import Data.Maybe

import Debug.Trace

main :: IO ()
main = do
  input <- getParsedLines 9 (fromIntegral <$> number)
  print $ part1 input
  print $ part2 input

type Input  = [Int]
type Output = Int

part1 :: Input -> Output
part1 input = let (buf, lst) = splitAt 25 input
               in fromJust $ findHole buf lst

findHole :: [Int] -> [Int] -> Maybe Int
findHole _ [] = Nothing
findHole s@(_:ls) (i:is) = case twosum s i of
                             Just _ -> let s' = snoc i ls in findHole s' is
                             Nothing -> Just i

snoc a = foldr (:) [a]

twosum ns t = listToMaybe [ x | (x:ys) <- tails ns, y <- ys, let k = x + y, k == t]

part2 :: Input -> Output
part2 input = let badNum = traceShowId $ part1 input
                  range = fromJust . find (not . null) $ [solvePart2 badNum ts | ts <- tails input]
                  (fst, lst) = (minimum range, maximum range)
               in fst+lst

solvePart2 _ [] = []
solvePart2 _ [_] = []
solvePart2 t (l:l':ls)
  | t < 0     = []
  | t-l == l' = [l,l']
  | otherwise = let t' = traceShowId $ t-l
                    rng = solvePart2 t' (l':ls)
                 in if null rng then []
                                else traceShowId $ l : rng

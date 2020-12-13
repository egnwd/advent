{-|
   Name: Shuttle Search
   Url: <https://adventofcode.com/2020/day/13>
-}

module Day13 (main) where

import Advent
import Prelude hiding (unlines)

import Control.Applicative
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = do
  input1 <- getParsedInput 13 parseInput1
  input2 <- getParsedInput 13 parseInput2
  print $ (part1 input1, part2 input2)

type Time = Int
type Freq = Int
type Input1  = (Time, [Freq])
type Input2  = [Maybe Freq]
type Output = Int

-- | Parsing
parseInput1 :: Parser (Time, [Freq])
parseInput1 = do
  time <- fromIntegral <$> number <* newline
  freqs <- mapMaybe (fmap fromIntegral) <$> (Just <$> number <|> Nothing <$ char 'x') `sepBy` char ','
  return (time, freqs)

parseInput2 :: Parser [Maybe Freq]
parseInput2 = do
  number <* newline
  (Just . fromIntegral <$> number <|> Nothing <$ char 'x') `sepBy` char ','

part1 :: Input1 -> Output
part1 (now, freqs) = fetch $ zipWith (mod *** (ans now)) currTimes buses
  where
    ans now x y = (x-now) * y
    currTimes = concatMap (replicate (length freqs)) [now..]
    buses = cycle freqs

part2 :: Input2 -> Output
part2 input = search (pairs input) [s, s+j..]
  where
    s = search (topN input) [0..]
    j = jump . map fst . topN $ input
    topN = take 5 . pairs

fetch = snd . head . filter ((==0) . fst)
isOffset t bus off = (t+off) `mod` bus
pairs xs = mapMaybe id $ zipWith (liftA2 (,)) xs (map Just [0..])
jump = foldr lcm 1
check xs t = sum $ map (uncurry $ isOffset t) xs
search xs ts = fetch $ map (\t -> (check xs t, t)) ts

--- Note this is slightly different to Control.Arrow.(***)
(***) :: (Time -> Time -> a) -> (Time -> Time -> b) -> Time -> Time -> (a, b)
f *** g = \x y -> (f x y, g x y)

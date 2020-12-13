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
import Data.Foldable

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
part2 input = fst . foldl' step (0, 1) $ pairs input

fetch = snd . head . filter ((==0) . fst)
pairs xs = mapMaybe id $ zipWith (liftA2 (,)) xs (map Just [0..])

check (b, o) t = (t+o) `mod` b
search x ts = fetch $ map (\t -> (check x t, t)) ts

step (bAcc, oAcc) (b, o) = let bAcc' = search (b, o) [bAcc,bAcc+oAcc..]
                            in (bAcc', oAcc * b)

--- Note this is slightly different to Control.Arrow.(***)
(***) :: (Time -> Time -> a) -> (Time -> Time -> b) -> Time -> Time -> (a, b)
f *** g = \x y -> (f x y, g x y)

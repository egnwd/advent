{-|
   Name: Binary Boarding
   Url: <https://adventofcode.com/2020/day/5>
-}

module Main
  ( main
  ) where

import Advent
import Prelude hiding (unlines)

import Data.Semigroup
import Data.List
import Data.Word

import Debug.Trace

import Control.Monad.State

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.Megaparsec as MP

main :: IO ()
main = do
  input <- getParsedLines 5 parseInput
  print $ part1 input
  print $ part2 input

type Input  = [String]
type Output = Int

letters = S.fromList $ [ 'L', 'R', 'F', 'B' ]

-- | Parsing
parseInput :: Parser String
parseInput = T.unpack <$> MP.takeWhileP Nothing (`S.member` letters)

part1 :: Input -> Output
part1 = getMax . foldMap (Max . seatId)

part2 :: Input -> Output
part2 input = let x    = sort . map (seatId) $ input
                  diff = zipWith (-) (drop 1 x) x
               in (+1) . snd . head $ filter ((2 ==) . fst) $ zip diff x

seatId :: String -> Output
seatId s = let (s', (r, _r)) = runState  (getRow s)  (0, 127)
               (c, _c)       = execState (getCol s') (0, 7)
            in fromIntegral r * 8 + fromIntegral c

getRow :: String -> State (Word8, Word8) String
getRow ('F':r) = modify lowerHalf >> getRow r
getRow ('B':r) = modify upperHalf >> getRow r
getRow r = return r

getCol :: String -> State (Word8, Word8) String
getCol ('L':r) = modify lowerHalf >> getCol r
getCol ('R':r) = modify upperHalf >> getCol r
getCol r = return r

lowerHalf (l,u) = (l, u - halfway l u)
upperHalf (l,u) = (l + halfway l u, u)

halfway l u = ((u-l) `div` 2) + 1

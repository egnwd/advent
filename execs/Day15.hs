{-|
   Name: Rambunctious Recitation
   Url: <https://adventofcode.com/2020/day/15>
-}

module Day15 (main) where

import Advent
import Prelude hiding (unlines)
import Control.Arrow ((&&&))
import Control.Lens
import Data.Foldable

import Data.IntMap.Strict ((!))
import qualified Data.IntMap.Strict as M

import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char)

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedInput 15 parseInput

type Input  = [Int]
type Output = Int

data Acc = Acc { lastSeenN :: !Int, lastSeen :: M.IntMap Int } deriving Show

-- | Parsing
parseInput :: Parser [Int]
parseInput = (fromIntegral <$> number) `sepBy` char ','

part1 :: Input -> Output
part1 = solve 2020

part2 :: Input -> Output
part2 = solve 30000000

solve target input = lastSeenN $ foldl' go (Acc l startMap) [length fs..target-2]
  where
    (l:fs) = reverse input
    startMap = mkMap (reverse fs)

mkMap x = M.fromList $ zip x [0..]

go a@Acc{..} i = a { lastSeenN = x, lastSeen = lastSeen' }
  where
    x = if M.member lastSeenN lastSeen
           then i - (lastSeen ! lastSeenN)
           else 0
    lastSeen' = lastSeen & at lastSeenN ?~ i


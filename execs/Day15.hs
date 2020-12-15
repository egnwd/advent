{-|
   Name: Rambunctious Recitation
   Url: <https://adventofcode.com/2020/day/15>
-}

module Day15 (main) where

import Advent
import Prelude hiding (unlines)
import Control.Monad (foldM, zipWithM_)
import Control.Arrow ((&&&))
import Control.Monad.ST (runST)
import Data.Functor (($>))

import qualified Data.Vector.Unboxed.Mutable as V

import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char)

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedInput 15 parseInput

type Input  = [Int]
type Output = Int

-- | Parsing
parseInput :: Parser [Int]
parseInput = (fromIntegral <$> number) `sepBy` char ','

part1 :: Input -> Output
part1 = solve 2020

part2 :: Input -> Output
part2 = solve 30000000

solve target input = runST $
  do
    v <- V.new target
    let l = last input

    zipWithM_ (V.write v) input [1..]

    let f acc i =
          do
            j <- V.read v acc
            let acc' = if j == 0 then 0 else i - j
            V.write v acc i $> acc'

    foldM f l [length input..target-1]

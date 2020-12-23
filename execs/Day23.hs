{-|
   Name: Crab Cups
   Url: <https://adventofcode.com/2020/day/23>
-}

module Day23 (main) where

import Control.Arrow ((&&&))
import Control.Lens
import Data.Monoid
import Data.List.PointedList
import qualified Data.List.PointedList.Circular as PL
import Data.Maybe

main :: IO ()
main = print . (part1 &&& part2) $ parseInput

type Input  = PointedList Int
type Output = Int

-- | Parsing
parseInput :: PointedList Int
parseInput = fromJust . fromList $ [9,1,6,4,3,8,2,7,5]
testInput = fromJust . fromList $ [3,8,9,1,2,5,4,6,7]

numbers :: Int -> [Int]
numbers n = [n-1, n-2..1]++[9,8..n]

part1 :: Input -> Output
part1 = ans . (!! 100) . iterate move

part2 :: Input -> Output
part2 input = ans2 . (!! 10000000) . iterate move $ input & suffix %~ (++[10..1000000])

move :: PointedList Int -> PointedList Int
move input = let n = input ^. focus
                 top3 = take 3 (input ^. suffix ++ input ^. prefix)
                 rest = unsafeDelete . unsafeDelete . unsafeDelete . PL.next $ input
                 output = findNext rest n
              in PL.next . unsafeFind n $ output & suffix %~ (top3++)

unsafeDelete = fromJust . PL.delete
unsafeFind n = fromJust . find n

ans :: PointedList Int -> Int
ans input = let lst = unsafeFind 1 input
             in foldl1 (\acc n -> acc * 10 + n) (lst ^. suffix ++ lst ^. prefix)

ans2 :: PointedList Int -> Int
ans2 input = let lst = unsafeFind 1 input
                 a = PL.next lst ^. focus
                 b = PL.moveN 2 lst ^. focus
              in a * b

findNext input n = fromJust . getAlt . foldMap (Alt . (`find` input)) $ numbers n

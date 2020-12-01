{-|
   Name: Expense Reports
   Url: <https://adventofcode.com/2020/day/1>
-}
module Main (main) where

import Advent

main :: IO ()
main = do
  input <- getParsedLines 1 number
  print $ expenseReport input
  print $ expenseReport3 input

-- | Find two numbers in list that sum to 2020, then multiply the answers
--
-- >>> expenseReport [1721, 979, 366, 299, 675, 1456]
-- 514579
expenseReport :: [Integer] -> Integer
expenseReport ns = head [ x * y | x <- ns, y <- ns, let k = x + y, k == 2020]

-- | Find three numbers in list that sum to 2020, then multiply the answers
--
-- >>> expenseReport3 [1721, 979, 366, 299, 675, 1456]
-- 241861950
expenseReport3 :: [Integer] -> Integer
expenseReport3 ns = head [ x * y * z | x <- ns, y <- ns, z <- ns, let k = x + y + z, k == 2020]

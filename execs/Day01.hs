{-|
   Name: Expense Reports
   Url: <https://adventofcode.com/2020/day/1>
-}
module Main (main) where

import Advent
import Text.Megaparsec

main :: IO ()
main = do
  input <- getParsedLines 1 number
  print $ expenseReport input

-- | Find two numbers in list that sum to 2020, then multiply the answers
--
-- >>> expenseReport [1721, 979, 366, 299, 675, 1456]
-- 514579
expenseReport :: [Integer] -> Integer
expenseReport ns = head [ x * y | x <- ns, y <- ns, let k = x + y, k == 2020]

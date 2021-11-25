-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.

module AOC.Challenge.Day17 (
    day17a
  , day17b
  ) where

import AOC.Solver ((:~>)(..), dyno_)
import AOC.Common (parseLines, pDecimal, countTrue)

solve :: Int -> [Int] -> [[Int]]
solve _ [] = []
solve target (c : cs)
  | c > target = solve target cs
  | c == target = [c] : solve target cs
  | otherwise = map (c:) (solve (target - c) cs) ++ solve target cs

day17a :: [Int] :~> _
day17a = MkSol
    { sParse = parseLines pDecimal
    , sShow  = show
    , sSolve = Just . length . solve (dyno_ "litres" 150)
    }

day17b :: _ :~> _
day17b = MkSol
    { sParse = parseLines pDecimal
    , sShow  = show
    , sSolve = \cs -> let pos = solve (dyno_ "litres" 150) cs
                          mn = minimum $ map length pos
                       in Just $ countTrue ((==mn) . length) pos
    }

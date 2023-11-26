-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.

module AOC.Challenge.Day06 (
    day06a
  , day06b
  ) where

import AOC.Solver ((:~>)(..))
import Text.Read  (readMaybe)
import qualified Data.Vector as V
import qualified Data.Map as M

run :: [Int] -> (Int, Int)
run = positionOfLoop . iterate go . V.fromList
    where
        go ns = let imx = V.maxIndex ns
                    mx  = ns V.! imx
                 in V.accum (+) (ns V.// [(imx, 0)]) [(i `mod` V.length ns, 1) | i <- [imx + 1 .. imx + mx]]

positionOfLoop :: [V.Vector Int] -> (Int, Int)
positionOfLoop = go 0 M.empty
    where
        go :: Int -> M.Map (V.Vector Int) Int -> [V.Vector Int] -> (Int, Int)
        go _ _ [] = undefined
        go l m (x:xs) = case M.lookup x m of
                          Just n -> (l,n)
                          Nothing -> go (l+1) (M.insert x 1 (succ <$> m)) xs

day06a :: [Int] :~> Int
day06a = MkSol
    { sParse = traverse readMaybe . words
    , sShow  = show
    , sSolve = Just . fst . run
    }

day06b :: [Int] :~> Int
day06b = MkSol
    { sParse = traverse readMaybe . words
    , sShow  = show
    , sSolve = Just . snd . run
    }

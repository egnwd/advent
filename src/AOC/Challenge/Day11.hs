-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day11 (
    day11a
  , day11b
  ) where

import           AOC.Solver ((:~>)(..), dyno_)
import           AOC.Common (freqs, (!?))

import           Control.Lens                   ((^..), each, to)
import           Data.Map                       (Map)
import           Data.Semigroup                 (Sum(..))
import           Text.Read                      (readMaybe)
import           Data.Foldable                  (fold)
import qualified Data.Map                       as M

digits :: Int -> Int
digits = length . show

step :: Map Int (Sum Int) -> Map Int (Sum Int)
step = M.foldrWithKey (\k c -> M.unionWith (<>) $ go k c) M.empty
    where
        go :: Int -> Sum Int -> Map Int (Sum Int)
        go 0 c = M.singleton 1 c
        go n c = let cnt = digits n
                     e = cnt `div` 2
                  in if even cnt
                        then M.fromListWith (<>) $ n `divMod` (10^e) ^.. each . to (,c)
                        else M.singleton (n*2024) c

day11 :: Int -> [Int] :~> Int
day11 n = MkSol
    { sParse = traverse readMaybe . words
    , sShow  = show
    , sSolve = fmap (getSum . fold) . (!? dyno_ "blink" n) . iterate step . M.map Sum . freqs
    }

day11a :: [Int] :~> Int
day11a = day11 25

day11b :: [Int] :~> Int
day11b = day11 75

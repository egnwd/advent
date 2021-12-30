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

import           AOC.Solver ((:~>)(..))
import           AOC.Common (revFreq)
import           Data.List  (transpose)
import qualified Data.IntMap as M
import qualified Data.Set.NonEmpty as NES

type CSet = NES.NESet Char
type RevFreq a = M.IntMap (NES.NESet a)

day06 :: (RevFreq Char -> Maybe (CSet, RevFreq Char)) -> [String] :~> String
day06 f = MkSol
    { sParse = Just . transpose . lines
    , sShow  = id
    , sSolve = traverse (fmap (NES.findMax . fst) . f . revFreq)
    }

day06a :: [String] :~> String
day06a = day06 M.maxView

day06b :: [String] :~> String
day06b = day06 M.minView

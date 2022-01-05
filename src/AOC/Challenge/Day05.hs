-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.

module AOC.Challenge.Day05 (
    day05a
  , day05b
  ) where

import           AOC.Common  (loopMaybe, pDecimal, parseLines)
import           AOC.Solver  ((:~>) (..))
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM

parser :: String -> Maybe (IntMap Int)
parser = fmap (IM.fromList . zip [0..]) . parseLines pDecimal

solve :: (Int -> Int -> Int) -> IntMap Int -> Int
solve f js0 = fst $ loopMaybe (uncurry go) (0, js0)
    where
        go :: Int -> IntMap Int -> Maybe (Int, IntMap Int)
        go p js = (\j -> (p+j, IM.adjust (f j) p js)) <$> IM.lookup p js

day05a :: IntMap Int :~> Int
day05a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . solve (const succ)
    }

day05b :: IntMap Int :~> Int
day05b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . solve adjustJump
    }
    where
        adjustJump j
          | j >= 3 = pred
          | otherwise = succ

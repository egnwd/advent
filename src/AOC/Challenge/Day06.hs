-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day06 (
    day06a
  , day06b
  ) where

import           AOC.Solver     ((:~>)(..))
import           Data.Bifunctor (second)
import           Data.List      (tails)
import           Control.Lens   (preview, _head, _1, _last)
import qualified Data.Set       as S

findStartingPoint :: Ord a => Int -> [a] -> Maybe Int
findStartingPoint n =
    preview
        ( _head                     -- Get first window satisfying the predicate
        . _1                        -- Get the indicies of the elements
        . _last)                    -- Get the last index (the answer!)
    . filter ((==n) . S.size . snd) -- Filter to those windows of the correct size after deduplicating
    . map
        ( second S.fromList         -- Make a set out of the elements
        . unzip                     -- Make a tuple of the indicies and the elements
        . take n)                   -- Force the windows to size `n`
    . tails                         -- Make list of sliding windows
    . zip [1..]                     -- Add index

day06 :: Int -> String :~> Int
day06 n = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = findStartingPoint n
    }

day06a :: String :~> Int
day06a = day06 4

day06b :: String :~> Int
day06b = day06 14

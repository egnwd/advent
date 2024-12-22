-- |
-- Module      : AOC.Challenge.Day22
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 22.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day22 (
    day22a
  , day22b
  ) where

import           AOC.Prelude

import qualified Data.Map                       as M
import           Data.Bits
import           Control.Lens
import           Linear (V4(..))

prune :: Int -> Int
prune = (`mod` 16777216)

step :: Int -> Int
step a = let a' = prune $ (a * 64) `xor` a
             a'' = prune $ (a' `div` 32) `xor` a'
          in prune $ (a'' * 2048) `xor` a''

bananaMap :: Integral a => [a] -> Map (V4 a) a
bananaMap xs = M.fromListWith (const id)
               . zipWith ((swap .) . (,)) (tail . tail . tail . tail $ xs)
               . (zipWith4 V4 <*> tail <*> (tail . tail) <*> (tail . tail . tail))
               . pairwise subtract $ xs

sell :: [Map (V4 Int) Int] -> Maybe Int
sell xs = maximumOf traverse (M.unionsWith (+) xs)

day22a :: [Int] :~> _
day22a = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = show
    , sSolve = fmap sum . traverse ((!? 2000) . iterate step)
    }

day22b :: [Int] :~> _
day22b = MkSol
    { sParse = traverse readMaybe . lines
    , sShow  = show
    , sSolve = sell . map (bananaMap . map (`mod` 10) . take (succ $ dyno_ "sales" 2000) . iterate step)
    }



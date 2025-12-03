-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import           AOC.Common       (listTup)
import           AOC.Solver       ((:~>)(..))
import           Control.Monad    ((<=<))
import           Text.Read        (readMaybe)
import           Data.List.Split  (splitOn, chunksOf)
import qualified Data.Set         as S

rangeToList :: Int -> Int -> [Int]
rangeToList start end = [start..end]

isInvalid :: Int -> String -> Bool
isInvalid d x = n `mod` d == 0 && sameChunks
    where
        sameChunks = (==1) . S.size . S.fromList $ chunksOf (n `div` d) x
        n = length x

isInvalidN :: String -> Bool
isInvalidN x = or [ isInvalid d x | d <- [2..length x]]

day02a :: [(Int, Int)] :~> Int
day02a = MkSol
    { sParse = traverse (listTup <=< traverse readMaybe . splitOn "-") . splitOn ","
    , sShow  = show
    , sSolve = Just . sum . concatMap (filter (isInvalid 2 . show) . uncurry rangeToList)
    }

day02b :: [(Int, Int)] :~> Int
day02b = MkSol
    { sParse = traverse (listTup <=< traverse readMaybe . splitOn "-") . splitOn ","
    , sShow  = show
    , sSolve = Just . sum . concatMap (filter (isInvalidN . show) . uncurry rangeToList)
    }

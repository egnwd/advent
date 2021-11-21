-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.

module AOC.Challenge.Day04 (
    day04a
  , day04b
  ) where

import AOC.Solver
import Crypto.Hash.MD5
import Data.ByteString as B (unpack)
import Data.ByteString.UTF8 as BS (ByteString, fromString)
import Data.ByteString.Char8 as BC (pack)

loopEither
    :: (a -> Either r a)
    -> a
    -> r
loopEither f = go
  where
    go !x = case f x of
      Left  r  -> r
      Right !y -> go y

meetsCondition :: Int -> BS.ByteString -> Bool
meetsCondition n s = all (== 0) (take half bytes) && (even n || bytes !! half < 16)
  where half = n `div` 2
        bytes = B.unpack s

solve :: Int -> BS.ByteString -> Int
solve zs s = loopEither go 0
  where
    go n = if meetsCondition zs $ hash (s <> (pack . show $ n))
              then Left n
              else Right (n+1)

day04a :: _ :~> _
day04a = MkSol
    { sParse = Just . BS.fromString
    , sShow  = show
    , sSolve = Just . solve (dyno_ "zeroes" 5)
    }

day04b :: _ :~> _
day04b = MkSol
    { sParse = Just . BS.fromString
    , sShow  = show
    , sSolve = Just . solve (dyno_ "zeroes" 6)
    }

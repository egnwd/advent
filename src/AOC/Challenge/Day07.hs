-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day07 (
    day07a
  , day07b
  ) where

import           Prelude hiding ((||))
import           AOC.Solver ((:~>)(..))
import           AOC.Common (parseLines, pTok, pDecimal, CharParser)
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P

parse :: Num a => CharParser (a, [a])
parse = (,) <$> (pDecimal <* pTok (P.char ':')) <*> P.many (pTok pDecimal)

canCreate :: Ord a => [a -> a -> a] -> a -> [a] -> Bool
canCreate ops test = elem test . go
    where
        go [] = mempty
        go [x] = return x
        go (a : b : xs) = do
            op <- ops
            case a `op` b of
              x
                | x > test -> mempty
                | otherwise -> go (x : xs)

(||) :: (Integral a) => a -> a -> a
a || b = let e = succ . (floor :: Double -> Int) . logBase 10 . fromIntegral $ b
          in a * (10 ^ e) + b

day07 :: [Int -> Int -> Int] -> [(Int, [Int])] :~> Int
day07 ops = MkSol
    { sParse = parseLines parse
    , sShow  = show
    , sSolve = Just . sum . map fst . filter (uncurry $ canCreate ops)
    }

day07a :: [(Int, [Int])] :~> Int
day07a = day07 [ (+), (*) ]

day07b :: [(Int, [Int])] :~> Int
day07b = day07 [ (+), (*), (||) ]

{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import           AOC.Prelude
import GHC.Float

parser = traverse (\x -> readMaybe [x] :: Maybe Int)

solve xs = let a = solve' xs in [a, 1-a]
solve' xs | countTrue (==1) xs >= countTrue (==0) xs = 1
          | otherwise = 0

toDec = sum . map(\(a,b) -> (2 ** a) * fromIntegral b) . zip [0..] . reverse

solveb xs = double2Int $ toDec o * toDec c
    where
        o = loopEither oxygen (0, xs)
        c = loopEither co2 (0, xs)

oxygen (i, ns) = if length filtered == 1 then Left (head filtered) else Right (i+1, filtered)
    where
        common = map solve' (transpose ns)
        filtered = filter ((==(common !! i)) . (!! i)) ns

co2 (i, ns) = if length filtered == 1 then Left (head filtered) else Right (i+1, filtered)
    where
        common = map ((1-) . solve') (transpose ns)
        filtered = filter ((==(common !! i)) . (!! i)) ns

loopEither
    :: (a -> Either r a)
    -> a
    -> r
loopEither f = go
  where
    go !x = case f x of
      Left  r  -> r
      Right !y -> go y

day03a :: _ :~> Int
day03a = MkSol
    { sParse = traverse parser . lines
    , sShow  = show
    , sSolve = Just . double2Int . product . map toDec . transpose . map (solve . sort) . transpose
    }

day03b :: _ :~> _
day03b = MkSol
    { sParse = traverse parser . lines
    , sShow  = show
    , sSolve = Just . solveb
    }

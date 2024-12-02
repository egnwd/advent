{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

monotonic x = min (monotonicD x) (monotonicU x)

monotonicD [] = 0
monotonicD [_] = 0
monotonicD (x:y:xs)
  | x > y = monotonicD (y:xs)
  | otherwise = 1 + monotonicD (y:xs)

monotonicU [] = 0
monotonicU [_] = 0
monotonicU (x:y:xs)
  | x < y = monotonicU (y:xs)
  | otherwise = 1 + monotonicU (y:xs)

notTooMuch [] = 0
notTooMuch [_] = 0
notTooMuch (x:y:xs)
  | (abs $ x - y) >= 1 && (abs $ x - y) <= 3 = notTooMuch (y:xs)
  | otherwise = 1 + notTooMuch (y:xs)

violations x = monotonic x + notTooMuch x

withRemoval x = countTrue (\x' -> violations x' <= 0) xs
    where
        xs = zipWith (++) (inits x) (tail $ tails x)

day02a :: _ :~> _
day02a = MkSol
    { sParse = traverse (traverse (readMaybe :: String-> Maybe Int) . words) . lines
    , sShow  = show
    , sSolve = Just
             . countTrue (\x -> violations x <= 0)
    }

day02b :: _ :~> _
day02b = MkSol
    { sParse = sParse day02a
    , sShow  = show
    , sSolve = Just . countTrue (\x -> withRemoval x >= 1)
    }

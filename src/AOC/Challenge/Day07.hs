{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day07 (
    day07a
  , day07b
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
import qualified Data.Set.NonEmpty              as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

data Hands = FiveOf | FourOf | FullHouse | ThreeOf | TwoPair | Pair | HighCard deriving (Eq, Ord, Show)

data Label = A | K | Q | T | N9 | N8 | N7 | N6 | N5 | N4 | N3 | N2 | J deriving (Ord, Eq)

readLabel :: Char -> Label
readLabel = \case
    'A' -> A
    'K' -> K
    'Q' -> Q
    'J' -> J
    'T' -> T
    '9' -> N9
    '8' -> N8
    '7' -> N7
    '6' -> N6
    '5' -> N5
    '4' -> N4
    '3' -> N3
    '2' -> N2

compares :: [a -> a -> Ordering] -> a -> a -> Ordering
compares cs x y = mconcat $ map (\c -> c x y) cs

lookupInFreq = IM.lookup

hand :: [Char] -> Hands
hand (revFreq->cs)
  | isJust . lookupInFreq 5 $ cs = FiveOf
  | isJust . lookupInFreq 4 $ cs = FourOf
  | (isJust . lookupInFreq 2 $ cs) && (isJust . lookupInFreq 3 $ cs) = FullHouse
  | (isJust . lookupInFreq 3 $ cs) = ThreeOf
  | maybe False ((== 2) . S.size) . lookupInFreq 2 $ cs = TwoPair
  | isJust . lookupInFreq 2 $ cs = Pair
  | otherwise = HighCard

jokerHand :: [Char] -> Hands
jokerHand cs = flip upgrade (freqs cs) . jokerHand' $ revFreq (filter (/='J') cs)
    where
        jokerHand' cs
          | isJust . lookupInFreq 5 $ cs = FiveOf
          | isJust . lookupInFreq 4 $ cs = FourOf
          | (isJust . lookupInFreq 2 $ cs) && (isJust . lookupInFreq 3 $ cs) = FullHouse
          | (isJust . lookupInFreq 3 $ cs) = ThreeOf
          | maybe False ((== 2) . S.size) . lookupInFreq 2 $ cs = TwoPair
          | isJust . lookupInFreq 2 $ cs = Pair
          | otherwise = HighCard

        upgrade FiveOf rs = FiveOf
        upgrade FourOf rs
          | lookupFreq 'J' rs == 1 = FiveOf
          | otherwise = FourOf
        upgrade FullHouse rs = FullHouse
        upgrade ThreeOf rs
          | lookupFreq 'J' rs == 1 = FourOf
          | lookupFreq 'J' rs == 2 = FiveOf
          | otherwise = ThreeOf
        upgrade TwoPair rs
          | lookupFreq 'J' rs == 1 = FullHouse
          | otherwise = TwoPair
        upgrade Pair rs
          | lookupFreq 'J' rs == 1 = ThreeOf
          | lookupFreq 'J' rs == 2 = FourOf
          | lookupFreq 'J' rs == 3 = FiveOf
          | otherwise = Pair
        upgrade HighCard rs
          | lookupFreq 'J' rs == 1 = Pair
          | lookupFreq 'J' rs == 2 = ThreeOf
          | lookupFreq 'J' rs == 3 = FourOf
          | lookupFreq 'J' rs >= 4 = FiveOf
          | otherwise = HighCard


highCard :: [Char] -> [Label]
highCard = map readLabel

rank :: [([Char], Int)] -> [([Char], Int)]
rank xs = sortBy (flip $ compares [comparing (hand . fst), comparing (highCard . fst)]) xs

jokerRank :: [([Char], Int)] -> [([Char], Int)]
jokerRank xs = sortBy (flip $ compares [comparing (jokerHand . fst), comparing (highCard . fst)]) xs

day07a :: _ :~> _
day07a = MkSol
    { sParse = traverse ((sequence . second (readMaybe @ Int)) <=< listTup . words) . lines
    , sShow  = show
    , sSolve = Just . sum . zipWith (\i (_, b) -> i * b) [1..] . rank
    }

day07b :: _ :~> _
day07b = MkSol
    { sParse = sParse day07a
    , sShow  = show
    , sSolve = Just . sum . zipWith (\i (_, b) -> i * b) [1..] . jokerRank
    }

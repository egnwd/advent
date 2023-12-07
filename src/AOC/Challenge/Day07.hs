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

data Card = A | K | Q | J | T | N9 | N8 | N7 | N6 | N5 | N4 | N3 | N2 | Joker deriving (Ord, Eq, Show)

instance Read Card where
    readsPrec _ = map ((, "") . readCard)

readCard :: Char -> Card
readCard = \case
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

compares cs x y = mconcat $ map (\c -> c x y) cs

hand :: [Card] -> Hands
hand (revFreq->cs)
  | isJust . IM.lookup 5 $ cs = FiveOf
  | isJust . IM.lookup 4 $ cs = FourOf
  | (isJust . IM.lookup 2 $ cs) && (isJust . IM.lookup 3 $ cs) = FullHouse
  | (isJust . IM.lookup 3 $ cs) = ThreeOf
  | maybe False ((== 2) . S.size) . IM.lookup 2 $ cs = TwoPair
  | isJust . IM.lookup 2 $ cs = Pair
  | otherwise = HighCard

jokerHand :: [Card] -> Hands
jokerHand cs = jokerHand' total
    where
        cards = filter (/= Joker) cs
        nJokers = countTrue (==Joker) cs
        total = case IM.maxViewWithKey (revFreq cards) of
             Just ((i, mx), rest) -> let (mxc, restc) = S.deleteFindMin mx
                                      in IM.insert (i+nJokers) (S.singleton mxc)
                                         . maybe rest (\s -> IM.insert i s rest)
                                         . S.nonEmptySet
                                         $ restc
             Nothing -> IM.singleton nJokers (S.singleton Joker)

        jokerHand' cs
          | isJust . IM.lookup 5 $ cs = FiveOf
          | isJust . IM.lookup 4 $ cs = FourOf
          | (isJust . IM.lookup 2 $ cs) && (isJust . IM.lookup 3 $ cs) = FullHouse
          | (isJust . IM.lookup 3 $ cs) = ThreeOf
          | maybe False ((== 2) . S.size) . IM.lookup 2 $ cs = TwoPair
          | isJust . IM.lookup 2 $ cs = Pair
          | otherwise = HighCard

-- rank :: [([Char], Int)] -> [([Char], Int)]
rank = sortBy $ flip $ compares [comparing (jokerHand . fst), comparing fst]

day07a :: [([Card], Int)] :~> _
day07a = MkSol
    { sParse = traverse (sequence . (bimap (map readCard) (readMaybe @ Int)) <=< listTup . words) . lines
    , sShow  = show
    , sSolve = Just . sum . zipWith (\i (_, b) -> i * b) [1..] . rank
    }

day07b :: _ :~> _
day07b = MkSol
    { sParse = sParse day07a
    , sShow  = show
    , sSolve = Just -- . rank . map (first (map toJoker))
                 . sum . zipWith (\i (_, b) -> i * b) [1..] . rank . map (first (map toJoker))
    }

toJoker J = Joker
toJoker a = a

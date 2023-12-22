{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day18
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 18.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day18 (
    day18a
  , day18b
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
import qualified Data.Set.NonEmpty              as NES
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

import Linear (V2(..))

data Plan = Plan Dir Int deriving (Eq, Show)

parseLineA = do
    d <- pTok $ P.choice $ zipWith (\a b -> a <$ P.char b) [North ..] "URDL"
    n <- pTok pDecimal
    _ <- P.between (P.char '(') (P.char ')') (P.char '#' *> P.takeWhileP Nothing isAlphaNum)
    return $ Plan d n

parseLineB = do
    _ <- pWord <* pWord
    n <- foldl' (\acc x -> acc * 16 + digitToInt x) 0 <$> (P.string "(#" *> P.takeP Nothing 5)
    d <- P.choice $ zipWith (\a b -> a <$ P.char b) [North ..] "3012"
    return $ Plan d n

dig :: [Plan] -> NE.NonEmpty Point
dig = snd . foldl' go (0, 0 NE.:| [])
    where
        go (start, trench) (Plan dir n) = let end :: Point = start + (n L.*^ dirVec dir)
                                           in (end, end NE.<| trench)

excavate (t NE.:| ts) = dugOut + trenchTunnel
    where
        -- shoelace seems to take into account one side of the trench...
        trenchTunnel = succ . (`div` 2 ) . sum $ zipWith (((sum . abs) .) . subtract) (t : ts) ts
        -- https://en.wikipedia.org/wiki/Shoelace_formula
        dugOut = (`div` 2) . abs . sum $ zipWith go (t : ts) (ts ++ [t])
        go (V2 x y) (V2 x' y') = x * y' - y * x'

day18a :: _ :~> _
day18a = MkSol
    { sParse = parseLines parseLineA
    , sShow  = show
    , sSolve = Just . excavate . dig
    }

day18b :: _ :~> _
day18b = MkSol
    { sParse = parseLines parseLineB
    , sShow  = show
    , sSolve = Just . excavate . dig
    }

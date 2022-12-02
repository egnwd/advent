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

data State = Win | Draw | Lose deriving (Eq, Ord, Show)

-- A = Rock
-- B = Paper
-- C = Scissors

scoreShape "A" = 1
scoreShape "B" = 2
scoreShape "C" = 3

scoreState Win = 6
scoreState Draw = 3
scoreState Lose = 0

mapScore "X" = "A"
mapScore "Y" = "B"
mapScore "Z" = "C"

mapOutcome "X" = Lose
mapOutcome "Y" = Draw
mapOutcome "Z" = Win

getOutcome "B" "A" = Lose
getOutcome "A" "C" = Lose
getOutcome "C" "B" = Lose
getOutcome a b
  | a == b = Draw
  | otherwise = Win

scoreGame a b = scoreShape b + scoreState (getOutcome a b)

scoreGame2 a b = let myGo = f b a in scoreShape myGo + scoreState b
    where
        f Draw a = a
        f Win  "A" = "B"
        f Lose "A" = "C"
        f Win  "B" = "C"
        f Lose "B" = "A"
        f Win  "C" = "A"
        f Lose "C" = "B"

day02a :: [_] :~> _
day02a = MkSol
    { sParse = Just . map (words) . lines
    , sShow  = show
    , sSolve = Just . sum . map (\[a,b] -> scoreGame a (mapScore b))
    }

day02b :: _ :~> _
day02b = MkSol
    { sParse = Just . map (words) . lines
    , sShow  = show
    , sSolve = Just . sum . map (\[a,b] -> scoreGame2 a (mapOutcome b))
    }

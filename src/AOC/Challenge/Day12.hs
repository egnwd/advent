{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day12 (
    day12a
  , day12b
  ) where

import           AOC.Prelude

import Data.Finite (finite)

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

data Land = Start | Middle Letter | End deriving (Show, Eq, Ord)

toLand 'S' = Just Start
toLand 'E' = Just End
toLand  c  = Middle . snd <$> charFinite c

shortestPath mp = bfs ns start end
    where
        start = head . M.keys . M.filter (== Start) $ mp
        end n = mp M.! n == End
        ns n = let h = mp M.! n
                   neighbours = S.fromList (((+n) . dirVec) <$> [North ..])
                   options = M.filterWithKey (\k v -> k `S.member` neighbours && littleEnergy h v) mp
                in M.keysSet options

bestHikingTrail mp = bfs ns start end
    where
        start = head . M.keys . M.filter (== End) $ mp
        end n = mp M.! n == Start || mp M.! n == Middle (finite 0)
        ns n = let h = mp M.! n
                   neighbours = S.fromList (((+n) . dirVec) <$> [North ..])
                   options = M.filterWithKey (\k v -> k `S.member` neighbours && littleEnergy v h) mp
                in M.keysSet options

littleEnergy a b = (height b - height a) <= 1
    where
        height Start = 0
        height End = 25
        height (Middle h) = fromIntegral h

day12a :: _ :~> _
day12a = MkSol
    { sParse = Just . parseAsciiMap toLand
    , sShow  = show
    , sSolve = fmap length . shortestPath
    }

day12b :: _ :~> _
day12b = MkSol
    { sParse = Just . parseAsciiMap toLand
    , sShow  = show
    , sSolve = fmap length . bestHikingTrail
    }

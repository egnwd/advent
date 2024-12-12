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
import           Data.Set.NonEmpty              (NESet)
import qualified Data.Set.NonEmpty              as NES
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

splitMaps :: Map Point Char -> Map Char (Set Point)
splitMaps = M.fromListWith S.union . map (\(a,b) -> (b, S.singleton a)) . M.toList

price :: Set (NESet Point) -> Int
price = sum . map (\x -> perim x * area x) . S.toList
    where
        perim :: NESet Point -> Int
        perim (NES.toSet->x) = sum . map (S.size . (`S.difference` x) . neighboursSet) . S.toList $ x
        area = NES.size

priceDiscount :: Set (NESet Point) -> Int
priceDiscount = sum . map (\x -> perim x * area x) . S.toList
    where
        perim :: NESet Point -> Int
        perim (NES.toSet->x) = sum $ map go [ North .. ]
            where
                go d = S.size . contiguousRegions . foldMap ((`S.difference` x) . S.singleton . (+dirVec d)) $ x
        area = NES.size

day12a :: _ :~> _
day12a = MkSol
    { sParse = Just . parseAsciiMap Just
    , sShow  = show
    , sSolve = Just . sum . M.map (price . contiguousRegions) . splitMaps
    }

day12b :: _ :~> _
day12b = MkSol
    { sParse = sParse day12a
    , sShow  = show
    , sSolve = Just . sum . M.map (priceDiscount . contiguousRegions) . splitMaps
    }

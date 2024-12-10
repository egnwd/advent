{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day10 (
    day10a
  , day10b
  ) where

import           AOC.Prelude hiding (insert)

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
import           Linear                         (V2(..))
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP
import Data.Finite (Finite, weaken, strengthen)
import Control.Lens (preview)

next :: Map Point (Finite 10) -> Point -> Set Point
next mp p = S.filter (isValid (nextLevel p) . (`M.lookup` mp)) . neighboursSet $ p
    where
        isValid (Just a) (Just b) = a == b
        isValid _ _ = False
        nextLevel a = strengthen . succ . weaken =<< M.lookup a mp


searchTrailheads :: Map Point (Finite 10) -> _
searchTrailheads mp = map trailheadScore starts
    where
        starts = M.keys . M.filter (== 0) $ mp
        trailheadScore = M.size . M.filter (== 9) . M.restrictKeys mp . go S.empty . S.singleton
            where
                go !innr !outr
                    | S.null outr = innr
                    | otherwise = go innr' outr'
                    where
                        innr' = S.union innr outr
                        outr' = foldMap (next mp) outr `S.difference` innr'

searchNumberOfTrails :: Map Point (Finite 10) -> Int
searchNumberOfTrails mp = getSum . foldMap (fold . buildTrail) . M.keys . M.filter (== 0) $ mp
    where
        buildTrail p
          | mp M.! p == 9 = return $ Sum 1
          | otherwise = S.toList (next mp p) >>= buildTrail


day10a :: Map Point (Finite 10) :~> Int
day10a = MkSol
    { sParse = Just . parseAsciiMap (preview decDigit)
    , sShow  = show
    , sSolve = Just . sum . searchTrailheads
    }

day10b :: Map Point (Finite 10) :~> Int
day10b = MkSol
    { sParse = Just . parseAsciiMap (preview decDigit)
    , sShow  = show
    , sSolve = Just . searchNumberOfTrails
    }

{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day20
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 20.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day20 (
    day20a
  , day20b
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
import           Data.Finite

data RaceTrack = Start | Track | End deriving (Show, Eq, Ord)

parse 'S' = Just Start
parse 'E' = Just End
parse '.' = Just Track
parse _ = Nothing

toPoints mp = do
    let points = M.fromListWith (<>) . map (\(k, x) -> (x, S.singleton k)) . M.toList $ mp
    [s] <- S.toList <$> M.lookup Start points
    [e] <- S.toList <$> M.lookup End points
    return (s, e, fold points)

race :: Point -> Point -> Set Point -> Maybe _
race start end mp = aStar' next cost ((== end) . fst) (start, 2)
    where
        cost = manhattan end . fst
        next :: (Point, Finite 3) -> Map (Point, Finite 3) Int
        next (p, 0) = M.fromSet (const 1) . S.map (,0) $ neighboursSet p `S.intersection` mp
        next (p, 1) = M.fromSet (const 1) . S.map (,0) $ neighboursSet p
        next (p, 2) = let nx = neighboursSet p
                          cheats = S.map (,1) nx
                          regular = S.map (,2) (nx `S.intersection` mp)
                       in M.fromSet (const 1) $ S.union cheats regular
        next (_, _) = undefined

day20a :: _ :~> _
day20a = MkSol
    { sParse = toPoints . parseAsciiMap parse
    , sShow  = show
    , sSolve = \(s, e, m) -> race s e m
    }

day20b :: _ :~> _
day20b = MkSol
    { sParse = toPoints . parseAsciiMap parse
    , sShow  = show
    , sSolve = Just
    }

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
import           Data.Functor.Foldable

data RaceTrack = Start | Wall | End deriving (Show, Eq, Ord)

parse :: Char -> Maybe RaceTrack
parse 'S' = Just Start
parse 'E' = Just End
parse '#' = Just Wall
parse _ = Nothing

toPoints :: Ord b => Map b RaceTrack -> Maybe (b, b, Set b)
toPoints mp = do
    let points = M.fromListWith (<>) . map (\(k, x) -> (x, S.singleton k)) . M.toList $ mp
    [s] <- S.toList <$> M.lookup Start points
    [e] <- S.toList <$> M.lookup End points
    return (s, e, M.keysSet . M.filter (== Wall) $ mp)

race :: Int -> Point -> Point -> Set Point -> Maybe (Sum Int)
race cheat start end ws = do
    (mn, path) <- aStar' next (manhattan end) (== end) start
    return $ foldMap (go mn $ M.fromList $ zip path [0..]) path
        where
            next p = M.fromSet (const 1) $ neighboursSet p `S.difference` ws
            cheats :: Set Point
            cheats = fixedPoint (foldMap (S.filter ((<= cheat) . manhattan 0) . (S.insert <*> neighboursSet))) (S.singleton 0)
            go :: Int -> Map Point Int -> Point -> Sum Int
            go mn pth p = Sum
                        . M.size
                        . M.filterWithKey (\k i -> mn - ((pth M.! p) + manhattan p k + (mn - i)) >= 100)
                        $ pth `M.restrictKeys` S.mapMonotonic (+p) cheats

day20 :: Int -> (Point, Point, Set Point) :~> Int
day20 n = MkSol
    { sParse = toPoints . parseAsciiMap parse
    , sShow  = show
    , sSolve = \(s, e, m) -> getSum <$> race n s e m
    }

day20a :: (Point, Point, Set Point) :~> Int
day20a = day20 2

day20b :: (Point, Point, Set Point) :~> Int
day20b = day20 20

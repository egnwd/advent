{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
import qualified Data.Set.NonEmpty       as NES
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP
import Linear

data Pipe = Animal | V | H | SW | SE | NE | NW deriving (Show, Eq, Enum, Bounded)

pipeNeighboursSet d p k = S.fromList [k + k' | k' <- allowed p, sum (abs k') == 1, k' /= pure 0]
    where
        allowed Animal = d
        allowed V = [V2   0 (-1), V2   0   1]
        allowed H = [V2 (-1)  0,  V2   1   0]
        allowed SW = [V2  0 (-1), V2   1   0]
        allowed SE = [V2  0 (-1), V2 (-1)  0]
        allowed NE = [V2  0   1,  V2 (-1)  0]
        allowed NW = [V2  0   1,  V2   1   0]

floodFillCount
    :: Ord a
    => (a -> Set a)     -- ^ Expansion (be sure to limit allowed points)
    -> Set a            -- ^ Start points
    -> (Int, Set a)     -- ^ Flood filled, with count of number of steps
floodFillCount f = go 0 S.empty
  where
    go !n !innr !outr
        | S.null outr' = (n, innr')
        | otherwise    = go (n + 1) innr' outr'
      where
        innr' = S.union innr outr
        outr' = foldMap f outr `S.difference` innr'

parsePipe :: Char -> Maybe Pipe
parsePipe = flip M.lookup . M.fromList $ zip "S|-LJ7F" [Animal .. NW]

run mp = floodFillCount (\p -> M.keysSet . M.restrictKeys mp . pipeNeighboursSet ds (mp M.! p) $ p) (S.singleton s)
    where
        allDs = sequence (pure [-1, 0, 1])
        ok a = a /= pure 0 && sum (abs a) == 1
        validRoute a = case M.lookup (s+a) mp of
                         Nothing -> False
                         Just p -> s `S.member` pipeNeighboursSet [] p (s+a)
        ds = [a | a <- allDs, ok a, validRoute a]
        s = fst . head . filter ((==Animal) . snd) . M.toList $ mp

run2 :: Map Point Pipe -> (Int, Set Point)
run2 mp = second (`S.difference` pipe) $ floodFillCount (S.filter allowed . (`S.difference` pipe) . allNeighboursSet) pipe
    where
        pipe = M.keysSet mp
        allowed :: Point -> Bool
        allowed = inBoundingBox $ boundingBox allKeys
        Just allKeys = NE.nonEmpty . M.keys $ mp

containsEdge :: Map Point Pipe -> NES.NESet Point -> Bool
containsEdge mp = any (`S.member` edges) . NES.toList
    where
        edges = S.fromList $ concat $ [[V2 x ymn, V2 x ymx] | x <- [xmn .. xmx]] ++ [[V2 xmx y, V2 xmn y] | y <- [ymn .. ymx]]
        (V2 (V2 xmn ymn) (V2 xmx ymx)) = boundingBox allKeys
        Just allKeys = NE.nonEmpty . M.keys $ mp

doubleSize mp pipe = fillGaps . M.mapKeysMonotonic (*2) . M.restrictKeys mp $ pipe
    where
        allDs = sequence (pure [-1, 0, 1])
        ok a = a /= pure 0 && sum (abs a) == 1
        validRoute a = case M.lookup (s+a) mp of
                         Nothing -> False
                         Just p -> s `S.member` pipeNeighboursSet [] p (s+a)
        ds = [a | a <- allDs, ok a, validRoute a]
        s = fst . head . filter ((==Animal) . snd) . M.toList $ mp
        fillGaps = ap (foldr' go) M.toList
            where
                go (k, V) = M.insert (k+V2 0 (-1)) V . M.insert (k+V2 0 1) V
                go (k, H) = M.insert (k+V2 (-1) 0) H . M.insert (k+V2 1 0) H
                go (k, SW) = M.insert (k+V2 0 (-1)) V . M.insert (k+V2 1 0) H
                go (k, SE) = M.insert (k+V2 0 (-1)) V . M.insert (k+V2 (-1) 0) H
                go (k, NE) = M.insert (k+V2 0 1) V . M.insert (k+V2 (-1) 0) H
                go (k, NW) = M.insert (k+V2 0 1) V . M.insert (k+V2 1 0) H
                go (k, Animal) = id

day10a :: _ :~> _
day10a = MkSol
    { sParse = Just . parseAsciiMap parsePipe
    , sShow  = show
    , sSolve = Just . fst . run
    }

day10b :: _ :~> _
day10b = MkSol
    { sParse = sParse day10a
    , sShow  = show
    , sSolve = \mp -> Just . (\mp' -> sum . map (S.size . NES.filter (all even)) . filter (not . containsEdge mp') . toList . contiguousRegions . snd . run2 $ mp') . doubleSize mp . snd . run $ mp
    -- map (NES.size) . filter (not . containsEdge mp) . toList . contiguousRegions . snd . run2 mp . snd . run $ mp
    }

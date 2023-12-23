{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day23
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 23.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day23 (
    day23a
  , day23b
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

data Tile = Path | Forest | Slope Dir deriving (Show, Eq, Ord)

parseTile = \case
    '.' -> Just Path
    '#' -> Nothing -- Just Forest
    '^' -> Just (Slope North)
    '>' -> Just (Slope East)
    'v' -> Just (Slope South)
    '<' -> Just (Slope West)
    _ -> Nothing

parseTile2 = \case
    '.' -> Just Path
    '#' -> Nothing -- Just Forest
    '^' -> Just Path
    '>' -> Just Path
    'v' -> Just Path
    '<' -> Just Path
    _ -> Nothing

solve garden = go start S.empty 0
    where
        go :: Point -> Set Point -> Int -> [Int]
        go t seen tot = do
            (n,c) <- maybe [] M.toList $ M.lookup t graph
            guard (not $ S.member n seen)
            let seen' = S.insert n seen
            let tot' = tot + c
            if term (n, seen')
               then return tot'
               else go n seen' tot'
        graph = M.fromList $ (\x -> (x, length <$> bfsAll (\t -> if t == x || S.notMember t nodes then next t else S.empty) x (\t -> guard (t /= x && S.member t nodes) $> t) ((== S.size (next x)) . S.size))) <$> S.toList nodes
        nodes = S.union (S.fromList [start, term0]) $ M.keysSet $ M.filterWithKey (\k _ -> S.size (nei k) > 2) garden
        paths = M.keysSet garden
        next t = nei t
        nei t = case M.lookup t garden of
                  Just Path -> paths `S.intersection` neighboursSet t
                  Just (Slope d) -> paths `S.intersection` (S.singleton (dirVec d + t))
                  _ -> S.empty

        start = S.findMin paths
        term0 = S.findMax paths
        term = (== term0) . fst
        heur = negate . manhattan term0 . fst

day23a :: _ :~> _
day23a = MkSol
    { sParse = Just . parseAsciiMap parseTile
    , sShow  = show
    , sSolve = Just . maximum . solve
    }

day23b :: _ :~> _
day23b = MkSol
    { sParse = Just . parseAsciiMap parseTile2
    , sShow  = show
    , sSolve = Just . maximum . solve
    }

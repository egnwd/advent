{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day06 (
    day06a
  , day06b
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.Set.NonEmpty             as NES
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import           Linear                         (V2(..), _x, _y)
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP
import           Control.Lens ((^.))

data Thing = Guard Dir | Obstable deriving (Show, Eq, Ord)

parse :: Char -> Maybe Thing
parse '^' = pure (Guard North)
parse '#' = pure Obstable
parse _   = empty

solve :: Map Point Thing -> Maybe (Set Point)
solve mp = do
    obs <- NES.nonEmptySet . M.keysSet . M.filter (== Obstable) $ mp
    let box = boundingBox obs
    let [(currLoc, Guard currDir)] = M.toList . M.filter (== Guard North) $ mp
    let go seen loc dir = case getter loc dir obs of
                        Just nextLoc -> go (S.union (S.fromList $ lineTo (V2 loc nextLoc)) seen) nextLoc (dir <> East)
                        Nothing -> S.union (S.fromList $ lineTo (V2 loc (setEdge box loc dir))) seen
    return $ go S.empty currLoc currDir

getter :: Point -> Dir -> NES.NESet Point -> Maybe Point
getter currLoc North = fmap (+ dirVec (North <> South)) . S.lookupMax . NES.filter (\x -> x ^. _x == currLoc ^. _x && x ^. _y < currLoc ^. _y)
getter currLoc East = fmap (+ dirVec (East <> South)) . S.lookupMin . NES.filter (\x -> x ^. _y == currLoc ^. _y && x ^. _x > currLoc ^. _x)
getter currLoc South = fmap (+ dirVec (South <> South)) . S.lookupMin . NES.filter (\x -> x ^. _x == currLoc ^. _x && x ^. _y > currLoc ^. _y)
getter currLoc West = fmap (+ dirVec (West <> South)) . S.lookupMax . NES.filter (\x -> x ^. _y == currLoc ^. _y && x ^. _x < currLoc ^. _x)

solveB :: Map Point Thing -> Maybe _
solveB mp = do
    obs <- NES.nonEmptySet . M.keysSet . M.filter (== Obstable) $ mp
    let box = boundingBox obs
    let [(currLoc, Guard currDir)] = M.toList . M.filter (== Guard North) $ mp
    let go seen loc dir = case getter loc dir obs of
                        Just nextLoc -> go (S.union (S.fromList $ lineTo (V2 loc nextLoc)) seen) nextLoc (dir <> East)
                        Nothing -> S.union (S.fromList $ lineTo (V2 loc (setEdge box loc dir))) seen
    let possibles = go S.empty currLoc currDir `S.difference` M.keysSet mp
    return $ countTrue (\p -> causesLoop $ M.insert p Obstable mp) . S.toList $ possibles

causesLoop :: Map Point Thing -> _
causesLoop mp = fromMaybe False $ do
    obs <- NES.nonEmptySet . M.keysSet . M.filter (== Obstable) $ mp
    let [(currLoc, Guard currDir)] = M.toList . M.filter (== Guard North) $ mp
    let go seen loc dir = case getter loc dir obs of
                        Just nextLoc
                          | loc == nextLoc -> go (S.insert (dir, loc) seen) nextLoc (dir <> East)
                          | otherwise -> let next = S.union (S.fromList . map (dir,) $ lineTo (V2 loc nextLoc)) seen
                                          in seen == next || go next nextLoc (dir <> East)
                        Nothing -> False
    -- return $ (currLoc, currDir, obs)
    return $ go S.empty currLoc currDir

day06a :: _ :~> _
day06a = MkSol
    { sParse = Just . parseAsciiMap parse
    , sShow  = show
    , sSolve = fmap S.size . solve
    }

day06b :: _ :~> _
day06b = MkSol
    { sParse = Just . parseAsciiMap parse
    , sShow  = show
    , sSolve = solveB
    }

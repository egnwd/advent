{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day07 (
    day07a
  , day07b
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.Set.NonEmpty              as NES
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
import Control.Monad.State

import Linear (V2(..))

data ManifoldItem = Start | Splitter deriving (Eq, Show)

mapParser :: Char -> Maybe ManifoldItem
mapParser 'S' = Just Start
mapParser '^' = Just Splitter
mapParser _   = Nothing

part1 :: M.Map Point ManifoldItem -> Maybe Int
part1 = fmap S.size . part S.empty (\p l r -> S.insert p (l <> r))

part2 :: M.Map Point ManifoldItem -> Maybe Int
part2 = part 1 (const (+))

findStart :: M.Map Point ManifoldItem -> Maybe Point
findStart map0 = case M.keys $ M.filter (==Start) map0 of
                [s] -> Just s
                _ -> Nothing

part :: forall a. a -> (Point -> a -> a -> a) -> M.Map Point ManifoldItem -> Maybe a
part term agg map0 = do
    start <- findStart map0
    M.lookup start splitterMap
        where
            nextLayer = maybe term (splitterMap M.!) . nextSplitter
            splitters = M.fromListWith (<>)
                        . map (\(V2 a b) -> (a, NES.singleton b))
                        . M.keys
                        . M.filter (==Splitter)
                        $ map0

            splitterMap :: M.Map Point a
            splitterMap = M.mapWithKey eval map0

            nextSplitter :: Point -> Maybe Point
            nextSplitter (V2 sx sy) = fmap (V2 sx) . find (> sy) . NES.toAscList =<< M.lookup sx splitters

            eval :: Point -> ManifoldItem -> a
            eval s Start = nextLayer s
            eval s Splitter = agg s (nextLayer (s + dirVec West)) (nextLayer (s + dirVec East))

day07a :: _ :~> _
day07a = MkSol
    { sParse = Just . parseAsciiMap mapParser
    , sShow  = show
    , sSolve = part1
    }

day07b :: _ :~> _
day07b = MkSol
    { sParse = sParse day07a
    , sShow  = show
    , sSolve = part2
    }

{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day21
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 21.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day21 (
    day21a
  , day21b
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
import Control.Lens

data Plot = Start | Plot | Rock deriving (Show, Eq, Ord)

floodFill
    :: Ord a
    => (a -> Set a)     -- ^ Expansion (be sure to limit allowed points)
    -> Int              -- ^ Step count
    -> Set a            -- ^ Start points
    -> Maybe (Set a)    -- ^ Flood filled
floodFill f n = go n
  where
    go !n !outr
        | n == 0 = Just outr
        | S.null outr' = Nothing
        | otherwise    = go (n - 1) outr'
      where
        outr' = foldMap f outr

parsePlot = \case
    'S' -> pure Start
    '.' -> pure Plot
    '#' -> pure Rock
    _ -> Nothing

findStart = fst . head . M.toList . M.filter (== Start)
findPlots = M.keysSet . M.filter (/= Rock)

solve :: Int -> (Point, Set Point) -> _
solve n (s, garden) = allPaths
    where
        allPaths = floodFill paths n (S.singleton s)
        paths p = garden `S.intersection` neighboursSet p

day21a :: _ :~> _
day21a = MkSol
    { sParse = Just . bimap findStart findPlots . dupe . parseAsciiMap parsePlot
    , sShow  = show
    , sSolve = fmap S.size . solve (dyno_ "steps" 64)
    }

day21b :: _ :~> _
day21b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

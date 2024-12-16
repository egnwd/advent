{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day16 (
    day16a
  , day16b
  ) where

import           AOC.Prelude hiding (Space)

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

data ReindeerMap = Start | Space | End deriving (Show, Eq, Ord)

parse 'S' = Just Start
parse 'E' = Just End
parse '.' = Just Space
parse _ = Nothing

toPoints mp = do
    let points = M.fromListWith (<>) . map (\(k, x) -> (x, S.singleton k)) . M.toList $ mp
    [s] <- S.toList <$> M.lookup Start points
    [e] <- S.toList <$> M.lookup End points
    return (s, e, fold points)

solve :: (Point, Dir) -> Point -> Set Point -> Maybe (Int, [(Point, Dir)])
solve start end mp = aStar' next cost ((== end) . fst) start
    where
        next (p,d) = M.fromList [((p',d'),c) | ((p',d'), c) <- [((p, d <> East), 1000), ((p, d <> West), 1000), ((p + dirVec d, d), 1)], p' `S.member` mp]
        cost = manhattan end . fst

solve' :: (Point, Dir) -> Point -> Set Point -> Maybe _
solve' start end mp = aStarSeen next cost ((== end) . fst) start
    where
        next (p,d) = M.fromList [((p',d'),c) | ((p',d'), c) <- [((p, d <> East), 1000), ((p, d <> West), 1000), ((p + dirVec d, d), 1)], p' `S.member` mp]
        cost = manhattan end . fst

day16a :: _ :~> _
day16a = MkSol
    { sParse = toPoints . parseAsciiMap parse
    , sShow  = show
    , sSolve = \(s, e, mp) -> fst <$> solve (s, East) e mp
    }

day16b :: _ :~> _
day16b = MkSol
    { sParse = toPoints . parseAsciiMap parse
    , sShow  = show
    , sSolve = \(s, e, mp) -> S.size . foldMap (S.fromList . map fst) . snd <$> solve' (s, East) e mp
    }

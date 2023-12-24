{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day17 (
    day17a
  , day17b
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
import qualified Data.Set.NonEmpty              as NES
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP
import Linear (V2(..), (*^))
import Control.Lens

f (loc,(c,(d,n))) = ((loc,d,n),c)

makeNeighbours city = M.fromList
                    . map (\(loc,(c,(d,n))) -> ((loc,d,n),c))
                    . M.toList
                    . M.intersectionWith (,) city
                    . M.fromList

crucibleNeighbours city (a, d, n)
  | n == 3 = makeNeighbours city . map step $ [East, West]
  | otherwise = makeNeighbours city . map step $ [East, North, West]
    where
        step North = (a + dirVec d, (d, n+1))
        step rot = (a + dirVec (d <> rot), (d <> rot, 1))

ultraCrucibleNeighbours city (a, d, n)
  | n == 1 = let stops = traverse (`M.lookup` city) (lineTo $ V2 (a + dirVec d) (a + 3 * dirVec d))
              in maybe mempty (M.singleton (a + 3 * dirVec d, d, n+3)) (sum <$> stops)
  | n == 10 = makeNeighbours city . map step $ [East, West]
  | otherwise = makeNeighbours city . map step $ [East, North, West]
    where
        step North = (a + dirVec d, (d, n+1))
        step rot = (a + dirVec (d <> rot), (d <> rot, 1))

findLoss :: Int -> Map Point Int -> _ -> _
findLoss lo city next = minimumOf (traverse . _1) $ mapMaybe (aStar' next heur term . (start,,1)) [North ..]
    where
        Just (start `V2` end) = boundingBox <$> (NES.nonEmptySet . M.keysSet $ city)
        heur = manhattan end . view _1
        term :: (Point, Dir, Int) -> Bool
        term (a, _, n) = a == end && n >= lo

day17a :: Map Point Int :~> _
day17a = MkSol
    { sParse = Just . parseAsciiMap (readMaybe @ Int . pure)
    , sShow  = show
    , sSolve = findLoss 1 <*> crucibleNeighbours
    }

day17b :: Map Point Int :~> _
day17b = MkSol
    { sParse = Just . parseAsciiMap (readMaybe @ Int . pure)
    , sShow  = show
    , sSolve = findLoss 4 <*> ultraCrucibleNeighbours
    }

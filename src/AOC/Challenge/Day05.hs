{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day05 (
    day05a
  , day05b
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
import Data.IntervalMap.Strict (IntervalMap)
import qualified Data.Interval as IV
import qualified Data.IntervalMap.Strict as IVMap

mapToNext (dst,src,rng) x
  | src <= x && x <= (src+rng) = Just $ x + (dst - src)
  | otherwise                  = Nothing

listTup3 [a,b,c] = Just (a,b,c)
listTup3 _ = Nothing

parseData (s:mps) = sequenceTuple (seeds, mps')
    where
        seeds = traverse (readMaybe @ Int) . tail . words $ s
        mps' = traverse (traverse (listTup3 <=< traverse (readMaybe @ Int) . words) . tail . lines) mps

getNext :: [(Int, Int, Int)] -> Int -> Int
getNext mp = fromMaybe <$> id <*> asum . sequence (map mapToNext mp)

getNexts :: [(Int, Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
getNexts mp xs = go <> (map hack . IVMap.keys $ tmp IVMap.\\ mp')
    where
        hack :: IV.Interval Int -> _
        hack i = let (IV.Finite a) = IV.lowerBound i
                     (IV.Finite b) = IV.upperBound i
                  in (a,b)
        mp' = IVMap.fromList . map (\(a,b,c) -> (makeRange b (b + c), a - b)) $ mp
        makeRange a b = IV.Finite a IV.<=..<= IV.Finite b
        tmp = IVMap.fromList . map (,()) . map (uncurry makeRange) $ xs
        go = map (hack . (\(i,d) -> IV.mapMonotonic (+d) i)) . IVMap.toList $ IVMap.intersectionWith const mp' tmp

pairs [] = []
pairs (x:y:rest) = (x,x+y) : pairs rest

day05a :: ([Int], [[(Int, Int, Int)]]) :~> _
day05a = MkSol
    { sParse = parseData . splitOn "\n\n"
    , sShow  = show
    , sSolve = \(s, mps) -> minimumOf traverse . map (\x -> foldl' (flip getNext) x mps) $ s
    }

day05b :: _ :~> _
day05b = MkSol
    { sParse = parseData . splitOn "\n\n"
    , sShow  = show
    , sSolve = \(s, mps) -> minimumOf (traverse . traverse . _1) . map (\x -> foldl (flip getNexts) [x] mps) . pairs $ s
    }

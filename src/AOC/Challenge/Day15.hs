{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day15 (
    day15a
  , day15b
                           , remove
                           , splitInterval
  ) where

import           AOC.Prelude

import Linear

import Control.Lens
import Control.Arrow
import Data.Ix

import           Data.Interval ((<=..<=))
import Data.IntervalRelation
import qualified Data.Interval as I

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

parseSensor = do
    -- Sensor at x=2300471, y=2016823: closest beacon is at x=2687171, y=2822745
    pTok "Sensor at"
    x <- "x=" *> pDecimal <* pTok (P.char ',')
    y <- "y=" *> pDecimal <* pTok (P.char ':')
    return $ V2 x y

parseBeacon = do
    pTok "closest beacon is at"
    x <- "x=" *> pDecimal <* pTok (P.char ',')
    y <- "y=" *> pDecimal
    return $ V2 x y

findLocationsWithoutBeaconInRow :: Int -> _ -> _
findLocationsWithoutBeaconInRow r locs = beam - holes - beacons - sensors
    where
        beam = succ . I.width . I.hulls $ ranges
        holes = sum . map (succ . I.width) . foldl' remove [I.hulls ranges] $ ranges
        beacons = countTrue ((==r) . view _y) . S.fromList . map snd $ locs
        sensors = countTrue ((==r) . view _y) . S.fromList . map fst $ locs
        ranges = filter (not . I.null) . map (uncurry rowRange) $ locs
        rowRange :: Point -> Point -> I.Interval Int
        rowRange s@(V2 sx sy) b@(V2 bx by)
          = let offset = manhattan s b - abs (sy - r)
             in if offset < 0 then I.empty else (I.Finite (sx-offset) <=..<= I.Finite (sx+offset))

findHolesInRow :: Int -> _ -> _
findHolesInRow r locs = mapMaybe I.extractSingleton holes
    where
        holes = foldl' remove [I.hulls ranges] $ ranges
        ranges = filter (not . I.null) . map (uncurry rowRange) $ locs
        rowRange :: Point -> Point -> I.Interval Int
        rowRange s@(V2 sx sy) b@(V2 bx by)
          = let offset = manhattan s b - abs (sy - r)
             in if offset < 0 then I.empty else (I.Finite (sx-offset) <=..<= I.Finite (sx+offset))

remove is i = concatMap (filter (not . I.null) . splitInterval i) is
splitInterval i i'
  = case i `I.relate` i' of
       Overlaps -> [I.interval (I.upperBound i + 1, I.Closed) (I.upperBound' i')]
       Starts -> [I.interval (I.upperBound i + 1, I.Closed) (I.upperBound' i')]
       During -> [I.interval (I.lowerBound' i') (I.lowerBound i - 1, I.Closed), I.interval (I.upperBound i + 1, I.Closed) (I.upperBound' i')]
       Finishes -> [I.interval (I.lowerBound' i') (I.lowerBound i - 1, I.Closed)]
       Equal -> []
       FinishedBy -> []
       Contains -> []
       StartedBy -> []
       OverlappedBy -> [I.interval (I.lowerBound' i') (I.lowerBound i - 1, I.Closed)]
       _ -> [i'] -- not connected

day15a :: _ :~> _
day15a = MkSol
    { sParse = parseLines ((,) <$> parseSensor <*> parseBeacon)
    , sShow  = show
    , sSolve = Just . findLocationsWithoutBeaconInRow (dyno_ "key" 2000000)
    }

day15b :: _ :~> _
day15b = MkSol
    { sParse = parseLines ((,) <$> parseSensor <*> parseBeacon)
    , sShow  = show
    , sSolve = \locs -> fmap (\b -> 4000000 *  (b ^. _y) + (b ^. _x)) . preview _head . concatMap (\r -> map (V2 r) . findHolesInRow r $ locs) $ [0..(dyno_ "mxy" 4000000)]
    }

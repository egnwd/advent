{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day19
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 19.

module AOC.Challenge.Day19 (
    day19a
  , day19b
  ) where

import           AOC.Solver             ((:~>)(..))
import           AOC.Common
import           Control.Lens           (_1, _2, (^.))
import           Linear                 (V3(..))
import           Data.Bifunctor         (bimap)
import           Data.Functor.Compose   (Compose(..))
import           Data.Set.NonEmpty      (NESet)
import           Data.IntSet.NonEmpty   (NEIntSet, (\\))
import           Data.IntMap.NonEmpty   (NEIntMap, pattern IsEmpty, pattern IsNonEmpty)
import           Data.Map.NonEmpty      (NEMap)
import           Data.Set               (Set)
import           Data.List.NonEmpty     (NonEmpty)
import           Data.Semigroup         (First(..))
import           Data.Foldable          (toList)
import           Data.List              (uncons)
import           Control.Monad          ((<=<))
import qualified Data.Text as T
import qualified Data.IntMap as IM
import qualified Data.IntMap.NonEmpty as NEIM
import qualified Data.IntSet as IS
import qualified Data.IntSet.NonEmpty as NEIS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES

type ScannerId = Int
type BeaconLocations = NonEmpty Point3D
type BeaconLocationsSet = NESet Point3D

-- * Parsing

splitOn :: T.Text -> String -> [String]
splitOn s = map T.unpack . T.splitOn s . T.pack

parseScanner :: CharParser ScannerId
parseScanner = "--- scanner " *> pDecimal <* " ---"

parseCoord :: CharParser Point3D
parseCoord = V3 <$> (pDecimal <* ",") <*> (pDecimal <* ",") <*> pDecimal

parse :: String -> Maybe (NEIntMap BeaconLocations)
parse
  =  (NEIM.nonEmptyMap . IM.fromList)
 <=< traverse (sequenceTuple . bimap (parseMaybeLenient parseScanner) (NE.nonEmpty <=< (parseLines parseCoord . unlines)))
 <=< (traverse (uncons . lines) . splitOn "\n\n")

-- * Solving

allScans :: BeaconLocations -> NEMap D24 BeaconLocations
allScans scan = NEM.fromList $ (\o -> (o, orientPoint o <$> scan)) <$> allD24

solve :: NEIntMap BeaconLocations -> (BeaconLocationsSet, NEIntMap Vector3D)
solve = mapOutTrench . fmap allScans

mapOutTrench :: NEIntMap (NEMap D24 BeaconLocations) -> (BeaconLocationsSet, NEIntMap Vector3D)
mapOutTrench scans = go queue0 offsets0 orientations0 trench0
    where
        (id0, scans0) = NEIM.deleteFindMin scans ^. _1

        queue0 = rebuildQueue (NEIS.singleton id0)
        offsets0 = NEIM.singleton id0 0
        orientations0 = NEIM.singleton id0 (D24 North ZAxis False)
        trench0 = NES.fromList $ NEM.deleteFindMin scans0 ^. _1 . _2

        getMap (s,o) = (NEM.! o) . (NEIM.! s) $ scans

        rebuildQueue :: NEIntSet -> Set (ScannerId, D24)
        rebuildQueue seen = S.fromList $ (,) <$> IS.toList ids <*> toList allD24
            where
                ids = NEIM.keysSet scans \\ seen

        go
            :: Set (ScannerId, D24) -- ^ Remaining scans to process
            -> NEIntMap Vector3D    -- ^ Translations from the 'origin' scanner
            -> NEIntMap D24         -- ^ Scanner -> chosen orientation
            -> BeaconLocationsSet   -- ^ Current trench map
            -> (BeaconLocationsSet, NEIntMap Vector3D)
        go !queue !offsets !orientations !trench = case S.minView queue of
            Nothing -> (trench, offsets)
            Just (next@(i,o), queue') ->
                let nextMap = getMap next
                 in case findMatch nextMap (NEIM.mapWithKey (curry getMap) orientations) of
                      Nothing -> go queue' offsets orientations trench
                      Just (m, offset) -> go queue'' offsets' orientations' trench'
                          where
                              queue'' = rebuildQueue (NEIM.keysSet offsets')
                              offsetOfMatch = offsets NEIM.! m
                              offsets' = NEIM.insert i (offset + offsetOfMatch) offsets
                              orientations' = NEIM.insert i o orientations
                              offsetOfNext = offsets' NEIM.! i
                              trench' = trench <> NES.fromList ((+offsetOfNext) <$> nextMap)

        findMatch :: BeaconLocations -> NEIntMap BeaconLocations -> Maybe (Int, Vector3D)
        findMatch next choices = getFirst <$> NEIM.foldMapWithKey labelledMatch choices
            where
                labelledMatch k = getCompose . fmap (k,) . match next

        match :: BeaconLocations -> BeaconLocations -> Compose Maybe First Vector3D
        match next poss
          = case IM.filterWithKey (const . (>= 12)) . revFreq $ (-) <$> poss <*> next of
              IsEmpty -> Compose Nothing
              IsNonEmpty n -> let offset = (NES.findMin . fst . NEIM.maxView) n in pure offset

largestManhattan :: (Ord a, Foldable t, Num a, Num (t a)) => NEIntMap (t a) -> a
largestManhattan offsets = maximum $ manhattan <$> NEIM.elems offsets <*> NEIM.elems offsets

day19a :: NEIntMap BeaconLocations :~> Int
day19a = MkSol
    { sParse = parse
    , sShow  = show
    , sSolve = Just . NES.size . fst . solve
    }

day19b :: NEIntMap BeaconLocations :~> Int
day19b = MkSol
    { sParse = parse
    , sShow  = show
    , sSolve = Just . largestManhattan . snd . solve
    }

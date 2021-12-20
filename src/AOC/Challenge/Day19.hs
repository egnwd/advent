{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

-- |
-- Module      : AOC.Challenge.Day19
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 19.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day19 (
    day19a
  , day19b
                           , example
  ) where

import           AOC.Prelude hiding (Point, (\\))
import           Control.Lens hiding (uncons)
import           Linear hiding (trace)
import           Data.Bifunctor (bimap)
import Text.Heredoc
import           Data.Set.NonEmpty       (NESet)
import           Data.IntSet.NonEmpty    (NEIntSet, (\\))
import           Data.IntMap.NonEmpty    (NEIntMap, pattern IsEmpty, pattern IsNonEmpty)
import           Data.Map.NonEmpty       (NEMap)
import qualified Data.Text as T
import qualified Data.IntMap as IM
import qualified Data.IntMap.NonEmpty as NEIM
import qualified Data.IntSet as IS
import qualified Data.IntSet.NonEmpty as NEIS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES

-- * Parsing

splitOn :: T.Text -> String -> [String]
splitOn s = map T.unpack . T.splitOn s . T.pack

parseScanner :: CharParser Int
parseScanner = "--- scanner " *> pDecimal <* " ---"

parseCoord :: CharParser Point3D
parseCoord = V3 <$> (pDecimal <* ",") <*> (pDecimal <* ",") <*> pDecimal

sequenceTuple :: (Maybe a, Maybe b) -> Maybe (a,b)
sequenceTuple (Just a, Just b) = Just (a, b)
sequenceTuple _ = Nothing

parse :: String -> Maybe (NEIntMap (NonEmpty Point3D))
parse
  =  (NEIM.nonEmptyMap . IM.fromList)
 <=< traverse (sequenceTuple . bimap (parseMaybeLenient parseScanner) (NE.nonEmpty <=< (parseLines parseCoord . unlines)))
 <=< (traverse (uncons . lines) . splitOn "\n\n")

-- * Solving

allScans :: NonEmpty Point3D -> NEMap D24 (NonEmpty Point3D)
allScans scan = NEM.fromList $ (\o -> (o, orientPoint o <$> scan)) <$> allD24

solve :: NEIntMap (NonEmpty Point3D) -> _
solve scans = trench
    where
        forms = allScans <$> scans
        trench = mapOutTrench forms

mapOutTrench scans = go remaining (NEIM.singleton id0 0) (NEIM.singleton id0 (D24 North ZAxis False)) (NES.fromList scan0)
    where
        ((id0, scans0), scans1) = NEIM.deleteFindMin scans
        ((_, scan0), _) = NEM.deleteFindMin scans0
        remaining = rebuildQueue (NEIS.singleton id0)
        getMap (s,o) = (NEM.! o) . (NEIM.! s) $ scans

        rebuildQueue :: NEIntSet -> Set (Int, D24)
        rebuildQueue seen = S.fromList $ (,) <$> IS.toList ids <*> toList allD24
            where
                ids = NEIM.keysSet scans \\ seen

        go
            :: Set (Int, D24)    -- ^ remaining scans to process
            -> NEIntMap Vector3D -- ^ Translations from the 'origin' scanner
            -> NEIntMap D24      -- ^ Current trench map
            -> NESet Point3D
            -> (NESet Point3D, NEIntMap Vector3D)
        go !queue !offsets !mp !trench = case S.minView queue of
            Nothing -> (trench, offsets)
            Just (next@(i,o), queue') ->
                case findMatch (getMap next) (NEIM.mapWithKey (curry getMap) mp) of
                  Nothing -> go queue' offsets mp trench
                  Just (m, offset) -> go queue'' offsets' (NEIM.insert i o mp) (trench <> trench')
                      where
                          trench' = NES.fromList $ (+offsets' NEIM.! i) <$> getMap next
                          newTrench = traceShow ("old", trench) . traceShow ("new", trench') $ trench <> trench'
                          queue'' = rebuildQueue (NEIM.keysSet offsets')
                          offsets' = NEIM.insert i (offset + offsets NEIM.! m) offsets

        findMatch :: NonEmpty Point3D -> NEIntMap (NonEmpty Point3D) -> Maybe (Int, Vector3D)
        findMatch next choices = getFirst <$> NEIM.foldMapWithKey (`match` next) choices

        match :: Int -> NonEmpty Point3D -> NonEmpty Point3D -> Maybe (First (Int, Vector3D))
        match k this poss
          = case IM.filterWithKey (const . (>= 12)) . revFreq $ (-) <$> poss <*> this of
              IsEmpty -> Nothing
              IsNonEmpty n -> let offset = (NES.findMin . fst . NEIM.maxView) n in Just (First (k, offset))

day19a :: _ :~> _
day19a = MkSol
    { sParse = parse
    , sShow  = show
    , sSolve = Just . NES.size . fst . solve
    }

printTrench :: NESet Point3D -> String
printTrench = unlines . map printCoord . toList
    where
        printCoord :: Point3D -> String
        printCoord (V3 x y z) = show x ++ "," ++ show y ++ "," ++ show z

largestManhattan offsets = maximum $ (\x y -> sum . abs $ x - y) <$> toList offsets <*> toList offsets

day19b :: _ :~> _
day19b = MkSol
    { sParse = parse
    , sShow  = show
    , sSolve = Just . largestManhattan . snd . solve
    }

example :: String
example = drop 1 [here|
--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390|]

{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

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
                           , splitOn
  ) where

import           AOC.Prelude hiding (Point)
import           Control.Lens hiding (uncons)
import           Linear
import           Data.Bifunctor (bimap)
import           Data.Set.NonEmpty       (NESet)
import           Data.IntMap.NonEmpty    (NEIntMap)
import           Data.Map.NonEmpty       (NEMap)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import qualified Data.IntMap.NonEmpty as NEIM
import qualified Data.Map.NonEmpty as NEM

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
        trench = assembleTrench forms

assembleTrench scans0 = go remaining (NES.singleton (id0, D24 North ZAxis False))
    where
        ((id0, _), scans1) = NEIM.deleteFindMin scans0
        remaining = S.fromList $ (,) <$> IM.keys scans1 <*> toList allD24
        getMap (s,o) = (NEM.! o) . (NEIM.! s) $ scans0

        go
            :: Set (Int, D24)   -- ^ remaining scans to process
            -> NESet (Int, D24) -- ^ Current trench map
            -> NESet (Int, D24)
        go !queue !mp = case S.minView queue of
            Nothing -> mp
            Just (next@(i,_), queue') ->
                case find (match (getMap next)) (getMap <$> toList mp) of
                  Nothing -> go queue' mp
                  Just _ -> go (S.filter ((i /=) . fst) queue') (NES.insert next mp)

        match :: NonEmpty Point3D -> NonEmpty Point3D -> Bool
        match this poss = not . IM.null . IM.filterWithKey (\k _ -> k >= 12) . revFreq $ los
            where
                los = [ y - x | x <- toList this, y <- toList poss ]

day19a :: _ :~> _
day19a = MkSol
    { sParse = parse
    , sShow  = show
    , sSolve = Just . solve
    }

day19b :: _ :~> _
day19b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

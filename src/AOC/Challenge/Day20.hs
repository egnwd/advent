-- |
-- Module      : AOC.Challenge.Day20
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 20.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day20 (
    day20a
  , day20b
  ) where

import           AOC.Prelude

import qualified Data.Map                       as M
import qualified Data.Set                       as S

data RaceTrack = Start | Wall | End deriving (Show, Eq, Ord)

parse :: Char -> Maybe RaceTrack
parse 'S' = Just Start
parse 'E' = Just End
parse '#' = Just Wall
parse _ = Nothing

toPoints :: Ord b => Map b RaceTrack -> Maybe (b, b, Set b)
toPoints mp = do
    let points = M.fromListWith (<>) . map (\(k, x) -> (x, S.singleton k)) . M.toList $ mp
    [s] <- S.toList <$> M.lookup Start points
    [e] <- S.toList <$> M.lookup End points
    return (s, e, M.keysSet . M.filter (== Wall) $ mp)

race :: Int -> Point -> Point -> Set Point -> Maybe (Sum Int)
race cheat start end ws = do
    path <- snd <$> aStar' next (manhattan end) (== end) start
    return $ foldMap (go $ M.fromList $ zip path [0..]) path
        where
            next p = M.fromSet (const 1) $ neighboursSet p `S.difference` ws
            cheats = fixedPoint (foldMap (S.filter ((<= cheat) . manhattan 0) . (S.insert <*> neighboursSet))) (S.singleton 0)
            timeSaved enter enterTime exit endTime = endTime - enterTime - manhattan enter exit
            go pth enterCheat = Sum
                        . M.size
                        . M.filterWithKey (((>= 100) .) . timeSaved enterCheat (pth M.! enterCheat))
                        $ pth `M.restrictKeys` S.mapMonotonic (+enterCheat) cheats

day20 :: Int -> (Point, Point, Set Point) :~> Int
day20 n = MkSol
    { sParse = toPoints . parseAsciiMap parse
    , sShow  = show
    , sSolve = \(s, e, m) -> getSum <$> race n s e m
    }

day20a :: (Point, Point, Set Point) :~> Int
day20a = day20 2

day20b :: (Point, Point, Set Point) :~> Int
day20b = day20 20

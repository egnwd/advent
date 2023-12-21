{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module      : AOC.Challenge.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day16 (
    day16a
  , day16b
  ) where

import           AOC.Solver                             ((:~>)(..))
import           AOC.Common                             (Point, Dir(..), dirVec, parseAsciiMap, boundingBox)
import           Control.Lens                           (makeLenses, maximumOf, (%~))
import           Data.Function                          ((&))
import           Data.Map                               (Map)
import           Data.Set                               (Set)
import           Linear                                 (V2(..))
import qualified Data.Map                       as M
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Set.NonEmpty              as NES

data Mirror = NWSE | SWNE deriving (Eq, Ord)
data Splitter = V | H deriving (Eq, Ord)
data Tile = Empty | Mirror Mirror | Splitter Splitter deriving (Eq, Ord)

instance Show Tile where
    show Empty = "."
    show (Mirror m) = show m
    show (Splitter s) = show s

instance Show Mirror where
    show NWSE = "\\"
    show SWNE = "/"

instance Show Splitter where
    show V = "|"
    show H = "-"

type Lazer = (Point, Dir)

data EnergyState = EnergyState
    { _seenLazers :: !(Set Lazer)
    , _lazers :: !(Seq.Seq Lazer)
    } deriving (Eq, Ord, Show)

makeLenses ''EnergyState

parseObject :: Char -> Maybe Tile
parseObject = \case
    '|' -> Just (Splitter V)
    '-' -> Just (Splitter H)
    '/' -> Just (Mirror SWNE)
    '\\' -> Just (Mirror NWSE)
    '.' -> Just Empty
    _ -> Nothing

freshState :: Lazer -> EnergyState
freshState l = EnergyState (S.singleton l) (Seq.fromList [l])

allLazers :: Map Point Tile -> Maybe [Lazer]
allLazers tiles = do
    (V2 xmn ymn) `V2` (V2 xmx ymx) <- boundingBox <$> (NES.nonEmptySet . M.keysSet $ tiles)
    let starts = concat $ [[(V2 xmn y, East), (V2 xmx y, West)] | y <- [ymn..ymx]] ++ [[(V2 x ymn, South), (V2 x ymx, North)] | x <- [xmn..xmx]]
    return $ starts

findEnergized :: Map Point Tile -> Lazer -> Maybe Int
findEnergized tiles start = fmap (S.size . S.map fst) . go $ freshState start
    where
        go :: EnergyState -> Maybe (Set Lazer)
        go EnergyState{..} =
            case _lazers of
              Seq.Empty -> Just _seenLazers
              n Seq.:<| ns -> go . S.foldl' processNeighbor (EnergyState _seenLazers ns) $ ex n
        ex (lp, lh) = let lp' = lp + dirVec lh
                       in case M.lookup lp' tiles of
                            Just Empty        -> S.singleton (lp', lh)
                            Just (Mirror m)   -> S.singleton (lp', mirror m lh)
                            Just (Splitter s) -> S.fromList $ map (lp',) (splitter s lh)
                            Nothing           -> S.empty
        processNeighbor :: EnergyState -> Lazer -> EnergyState
        processNeighbor es0@EnergyState{..} x
          | x `S.member` _seenLazers = es0
          | otherwise                = es0 & (seenLazers %~ S.insert x) . (lazers %~ (Seq.:|> x))


mirror :: Mirror -> Dir -> Dir
mirror SWNE = \case
    North -> East
    East -> North
    West -> South
    South -> West
mirror NWSE = \case
    North -> West
    West -> North
    East -> South
    South -> East

splitter :: Splitter -> Dir -> [Dir]
splitter V = \case
    North -> [North]
    East -> [North, South]
    West -> [North, South]
    South -> [South]
splitter H = \case
    North -> [East, West]
    East -> [East]
    West -> [West]
    South -> [East, West]


day16a :: Map Point Tile :~> Int
day16a = MkSol
    { sParse = Just . parseAsciiMap parseObject
    , sShow  = show
    , sSolve = flip findEnergized (V2 0 0, East)
    }

day16b :: Map Point Tile :~> Int
day16b = MkSol
    { sParse = Just . parseAsciiMap parseObject
    , sShow  = show
    , sSolve = \tiles -> maximumOf traverse =<< traverse (findEnergized tiles) =<< allLazers tiles
    }

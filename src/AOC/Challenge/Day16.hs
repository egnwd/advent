{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE NamedFieldPuns #-}

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

import           AOC.Prelude hiding (Splitter)

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
import qualified Data.Sequence  as Seq
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP
import Control.Lens hiding (Empty)
import Control.Monad.State

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

allLazers tiles = do
    (L.V2 xmn ymn) `L.V2` (L.V2 xmx ymx) <- boundingBox <$> (NES.nonEmptySet . M.keysSet $ tiles)
    let starts = concat $ [[(L.V2 xmn y, East), (L.V2 xmx y, West)] | y <- [ymn..ymx]] ++ [[(L.V2 x ymn, South), (L.V2 x ymx, North)] | x <- [xmn..xmx]]
    return $ starts

runTilDone tiles start = go $ freshState start
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


day16a :: _ :~> _
day16a = MkSol
    { sParse = Just . parseAsciiMap parseObject
    , sShow  = show
    , sSolve = fmap (S.size . S.map fst) . flip runTilDone (L.V2 0 0, East)
    }

day16b :: _ :~> _
day16b = MkSol
    { sParse = Just . parseAsciiMap parseObject
    , sShow  = show
    , sSolve = \tiles -> maximumOf traverse =<< traverse (fmap (S.size . S.map fst) . runTilDone tiles) =<< allLazers tiles
    }

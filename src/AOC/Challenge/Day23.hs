-- |
-- Module      : AOC.Challenge.Day23
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 23.

module AOC.Challenge.Day23 (
    day23a
  , day23b
  ) where

import           AOC.Solver      ((:~>)(..))
import           AOC.Common      (Point, parseAsciiMap, displayAsciiMap, floydWarshall, aStar', manhattan)
import           Control.DeepSeq (NFData)
import           Control.Monad   (join)
import           Data.Bifunctor  (second)
import           Data.Map        (Map)
import           Data.Maybe      (isNothing)
import           Data.Semigroup  (Max(..), getMax)
import           GHC.Generics    (Generic)
import           Linear          (V2(..))
import qualified Data.Map as M

data Amphipod = Amber | Bronze | Copper | Desert deriving (Ord, Eq, Generic)
instance NFData Amphipod

instance Show Amphipod where
    show a  = [showAmphipod a]

showAmphipod :: Amphipod -> Char
showAmphipod Amber  = 'A'
showAmphipod Bronze = 'B'
showAmphipod Copper = 'C'
showAmphipod Desert = 'D'

newtype Maze = Maze { unMaze :: Map Point (Maybe Amphipod) } deriving (Eq, Ord, Generic)

instance NFData Maze

instance Show Maze where
    show = displayMaze

-- ^ Parsing

parser :: String -> Maze
parser = Maze . parseAsciiMap fn
    where
        fn :: Char -> Maybe (Maybe Amphipod)
        fn 'A' = Just (Just Amber)
        fn 'B' = Just (Just Bronze)
        fn 'C' = Just (Just Copper)
        fn 'D' = Just (Just Desert)
        fn _   = Just Nothing

insertExtraBurrows :: Maze -> Maze
insertExtraBurrows = Maze . M.union extraBurrows . M.mapKeys pushDown . unMaze
    where
        pushDown (V2 c 3) = V2 c 5
        pushDown p = p
        extraBurrows
          = M.fromList . map (second Just)
          $ [ (V2 3 3, Desert), (V2 5 3, Copper), (V2 7 3, Bronze), (V2 9 3, Amber)
            , (V2 3 4, Desert), (V2 5 4, Bronze), (V2 7 4, Amber), (V2 9 4, Copper)
            ]

-- ^ Helpers

energy :: Amphipod -> Int
energy = \case
    Amber  -> 1
    Bronze -> 10
    Copper -> 100
    Desert -> 1000

burrow :: Amphipod -> Int
burrow = \case
    Amber  -> 3
    Bronze -> 5
    Copper -> 7
    Desert -> 9

part1Graph :: Map Point (Map Point Int)
part1Graph = M.mapWithKey (\k -> M.fromList . map (\k' -> (k', manhattan k k'))) . M.fromList $
    [(V2 1 1,  [V2 2 1])
    ,(V2 2 1,  [V2 1 1, V2 3 2, V2 4 1])
    ,(V2 4 1,  [V2 2 1, V2 3 2, V2 5 2, V2 6 1])
    ,(V2 6 1,  [V2 4 1, V2 8 1, V2 5 2, V2 7 2])
    ,(V2 8 1,  [V2 6 1, V2 10 1, V2 7 2, V2 9 2])
    ,(V2 10 1, [V2 8 1, V2 11 1, V2 9 2])
    ,(V2 11 1, [V2 10 1])
    ,(V2 3 2,  [V2 2 1, V2 4 1, V2 3 3])
    ,(V2 5 2,  [V2 4 1, V2 6 1, V2 5 3])
    ,(V2 7 2,  [V2 6 1, V2 8 1, V2 7 3])
    ,(V2 9 2,  [V2 8 1, V2 10 1, V2 9 3])
    ,(V2 3 3,  [V2 3 2])
    ,(V2 5 3,  [V2 5 2])
    ,(V2 7 3,  [V2 7 2])
    ,(V2 9 3,  [V2 9 2])
    ]

part2Graph :: Map Point (Map Point Int)
part2Graph = M.mapWithKey (\k -> M.fromList . map (\k' -> (k', manhattan k k'))) . M.fromList $
    [(V2 1 1,  [V2 2 1])
    ,(V2 2 1,  [V2 1 1, V2 3 2, V2 4 1])
    ,(V2 4 1,  [V2 2 1, V2 3 2, V2 5 2, V2 6 1])
    ,(V2 6 1,  [V2 4 1, V2 8 1, V2 5 2, V2 7 2])
    ,(V2 8 1,  [V2 6 1, V2 10 1, V2 7 2, V2 9 2])
    ,(V2 10 1, [V2 8 1, V2 11 1, V2 9 2])
    ,(V2 11 1, [V2 10 1])
    ,(V2 3 2,  [V2 2 1, V2 4 1, V2 3 3])
    ,(V2 5 2,  [V2 4 1, V2 6 1, V2 5 3])
    ,(V2 7 2,  [V2 6 1, V2 8 1, V2 7 3])
    ,(V2 9 2,  [V2 8 1, V2 10 1, V2 9 3])
    ,(V2 3 3,  [V2 3 2, V2 3 4])
    ,(V2 5 3,  [V2 5 2, V2 5 4])
    ,(V2 7 3,  [V2 7 2, V2 7 4])
    ,(V2 9 3,  [V2 9 2, V2 9 4])
    ,(V2 3 4,  [V2 3 3, V2 3 5])
    ,(V2 5 4,  [V2 5 3, V2 5 5])
    ,(V2 7 4,  [V2 7 3, V2 7 5])
    ,(V2 9 4,  [V2 9 3, V2 9 5])
    ,(V2 3 5,  [V2 3 4])
    ,(V2 5 5,  [V2 5 4])
    ,(V2 7 5,  [V2 7 4])
    ,(V2 9 5,  [V2 9 4])
    ]

isHallwayTerminal :: Point -> Bool
isHallwayTerminal (V2 _ r) = r < 2

isBurrowTerminal :: Amphipod -> Point -> Bool
isBurrowTerminal a (V2 c _) = burrow a == c

solve :: Map Point (Map Point Int) -> Maze -> Maybe (Int, [Maze])
solve g = aStar' (allLegalMoves paths) totalTravelHome allAmphipodsHome
    where
        paths = floydWarshall g

totalTravelHome :: Maze -> Int
totalTravelHome = sum . map (\(p, a) -> maybe 0 (travelHome p) a) . M.toList . unMaze
    where
        travelHome p@(V2 c _) a = if burrow a == c then 0 else manhattan p (V2 (burrow a) 1)

allAmphipodsHome :: Maze -> Bool
allAmphipodsHome = M.null . M.filterWithKey (\(V2 c _) a -> maybe False (\a' -> burrow a' /= c) a) . unMaze

allLegalMoves :: Map Point (Map Point (Maybe (Int, [Point]))) -> Maze -> Map Maze Int
allLegalMoves paths mz@(Maze mp) = M.unions . map getLegalMoves . M.toList $ mp
    where
        !freePaths = M.filter (maybe False (allEmpty . snd)) <$> paths
        allEmpty = all (isNothing . join . flip M.lookup mp)
        outPaths = M.filterWithKey (const . isHallwayTerminal) . (freePaths M.!)
        inPaths a = M.filterWithKey (const . isBurrowTerminal a) . (freePaths M.!)
        inPaths' a from = let mx' = mx a from in M.filter ((mx' ==) . pathLength) . inPaths a $ from
        pathLength = maybe 0 (length . snd)
        mx a = getMax . foldMap (Max . pathLength) . inPaths a

        getLegalMoves :: (Point, Maybe Amphipod) -> Map Maze Int
        getLegalMoves (_, Nothing) = M.empty
        getLegalMoves (from@(V2 c r), Just a)
          | burrow a == c && allOf c a = M.empty
          | r > 1 = updateBurrows from (Just a) $ newMap a (outPaths from)
          | otherwise = updateBurrows from (Just a) $ newMap a (inPaths' a from)

        updateBurrows from a = M.mapKeys (\to -> Maze . M.insert to a . M.insert from Nothing . unMaze $ mz)
        newMap a = M.foldrWithKey (\k c -> M.alter (const ((*energy a) . fst <$> c)) k) M.empty
        allOf c a = all p [2..5]
            where
                p r = maybe True (==a) . join $ M.lookup (V2 c r) mp

day23a :: _ :~> _
day23a = MkSol
    { sParse = Just . parser
    , sShow  = show
    , sSolve = solve part1Graph
    }

day23b :: _ :~> _
day23b = MkSol
    { sParse = Just . insertExtraBurrows . parser
    , sShow  = show
    , sSolve = solve part2Graph
    }

displayMaze :: Maze -> String
displayMaze = ("\n" ++) . displayAsciiMap ' ' . fmap (maybe '.' (head . show)) . unMaze

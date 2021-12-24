{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : AOC.Challenge.Day23
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 23.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day23 (
    day23a
  , day23b
                           , example
  ) where

import           AOC.Prelude
import Linear
import Text.Heredoc
import Data.Functor.Foldable
import qualified Data.Map as M
import qualified Data.Set as S

data Amphipod = Amber | Bronze | Copper | Desert deriving (Ord, Eq, Generic)
instance NFData Amphipod

instance Show Amphipod where
    show Amber  = "A"
    show Bronze = "B"
    show Copper = "C"
    show Desert = "D"

newtype Maze = Maze { unMaze :: Map Point (Maybe Amphipod) } deriving (Eq, Ord, Generic)

instance NFData Maze

instance Show Maze where
    show = displayMaze

parser :: String -> Maze
parser = Maze . parseAsciiMap fn
    where
        fn :: Char -> Maybe (Maybe Amphipod)
        fn 'A' = Just (Just Amber)
        fn 'B' = Just (Just Bronze)
        fn 'C' = Just (Just Copper)
        fn 'D' = Just (Just Desert)
        fn _   = Just Nothing

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

burrowDoor :: Int
burrowDoor = 2

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

isHallwayTerminal (V2 _ r) = r < burrowDoor

isHallway _ (V2 _ 1) = True
isHallway (V2 c1 r1) (V2 c2 r2) = c1 == c2 && r1 > r2

isBurrowTerminal a  (V2 c _) = burrow a == c

isBurrow _  _ (V2 _ 1) = True
isBurrow a  _ (V2 c _) = burrow a == c

solve g = aStar' (allLegalMoves g) totalTravelHome allAmphipodsHome

-- totalTravelHome = const 0
totalTravelHome (Maze mz) = sum . map (\(p, a) -> maybe 0 (travelHome p) a) . M.toList $ mz
    where
        travelHome (V2 c r) a = undefined

allAmphipodsHome = M.null . M.filterWithKey (\(V2 c _) a -> maybe False (\a' -> burrow a' /= c) a) . unMaze

allLegalMoves :: Map Point (Map Point Int) -> Maze -> Map Maze Int
allLegalMoves g mz = M.unions . map getLegalMoves . M.toList . unMaze $ mz
    where
        getLegalMoves :: (Point, Maybe Amphipod) -> Map Maze Int
        getLegalMoves (_, Nothing) = M.empty
        getLegalMoves (from, Just a) = updateBurrows from (Just a) $ legalMoves g mz a from
        updateBurrows from a = M.mapKeys (\to -> Maze . M.update (const (Just a)) to . M.update (const (Just Nothing)) from . unMaze $ mz)

legalMoves :: Map Point (Map Point Int) -> Maze -> Amphipod -> Point -> Map Point Int
legalMoves g (Maze mz) a p@(V2 c r)
  | r > 1 = (*energy a) <$> evalState (go isHallway isHallwayTerminal p 0) (S.singleton p)
  | otherwise = (*energy a) <$> evalState (go (isBurrow a) (isBurrowTerminal a) p 0) (S.singleton p)
  where
      go :: _ -> _ -> Point -> Int -> State (Set Point) (Map Point Int)
      go pd pdt curr weight = do
          seen <- get
          let f next = isNothing (mz M.! next) && pd curr next && S.notMember next seen
          let queue = M.filterWithKey (const . f) $ g M.! curr
          modify (S.union (M.keysSet queue))
          let keeps = M.filterWithKey (const . pdt) queue
          fmap (M.map (+weight) . M.union keeps . M.unions) . traverse (uncurry (go pd pdt)) . M.toList $ queue

day23a :: _ :~> _
day23a = MkSol
    { sParse = Just . parser
    , sShow  = show -- ("\n" ++) . intercalate "\n" . map displayMaze
    , sSolve = fmap fst . solve part1Graph
    }

day23b :: _ :~> _
day23b = MkSol
    { sParse = Just . parser
    , sShow  = show
    , sSolve = fmap fst . solve part2Graph
    }

displayMaze :: Maze -> String
displayMaze = ("\n" ++) . displayAsciiMap ' ' . fmap (maybe '.' (head . show)) . unMaze

example = drop 1 [here|
#############
#...........#
###B#C#B#D###
  #D#C#B#A#
  #D#B#A#C#
  #A#D#C#A#
  #########|]

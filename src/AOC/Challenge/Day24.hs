-- |
-- Module      : AOC.Challenge.Day24
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable

module AOC.Challenge.Day24 (
    day24a
  , day24b
  ) where

import           AOC.Common
import           AOC.Solver ((:~>)(..))
import           Data.Foldable (foldl')
import           Data.Map (Map)
import           Linear (V2(..), _x, _y)
import           Control.Lens (over)
import           Control.Arrow ((&&&))
import           Data.Maybe
import qualified Data.Map                       as M
import qualified Data.Set                       as S
import qualified Data.Set.NonEmpty              as NES

data Tile = Open | Bliz Dir deriving (Eq, Ord)

instance Show Tile where
    show Open = "."
    show (Bliz North) = "^"
    show (Bliz East) = ">"
    show (Bliz South) = "v"
    show (Bliz West) = "<"

tile :: Char -> Maybe [Tile]
tile '.' = Just $ [Open]
tile '^' = Just $ [Open, Bliz North]
tile '>' = Just $ [Open, Bliz East]
tile 'v' = Just $ [Open, Bliz South]
tile '<' = Just $ [Open, Bliz West]
tile _ = Nothing

step :: V2 (V2 Int) -> Map Point [Tile] -> Map Point [Tile]
step edges grove = foldl' go M.empty . concatMap (\(p, ts) -> (p,) <$> ts) . M.toAscList $ grove
    where
        go :: Map Point [Tile] -> (Point, Tile) -> Map Point [Tile]
        go mp (p, t) = M.insertWith (++) (next t) [t] mp
            where
                next :: Tile -> Point
                next = \case
                    Open -> p
                    Bliz d -> wrap edges grove (p + dirVec d) d

wrap :: V2 (V2 Int) -> Map Point [Tile] -> Point -> Dir -> Point
wrap (V2 (V2 w n) (V2 e s)) mp p@(V2 x y) d
      | p `M.member` mp = p
      | otherwise       = case d of
          North -> V2 x s
          South -> V2 x n
          West  -> V2 e y
          East  -> V2 w y

findPath :: Bool -> Int -> Map Point [Tile] -> Maybe (Int, Map Point [Tile])
findPath forward t grove0 = ((+t) . length &&& (snd . last)) <$> bfs stepElves (S.singleton start, grove0) (S.member end . fst)
    where
        stepElves (elves, grove) = let g' = grove' grove
                                    in S.singleton (ns g' elves, g')
        ns grove = M.keysSet
                 . M.filter (== [Open])
                 . M.restrictKeys grove
                 . S.unions
                 . map (S.insert <*> neighboursSet)
                 . S.toList
        grove' = step edges

        Just edges = fmap (over (_y . _y) pred . over (_x . _y) succ . boundingBox) . NES.nonEmptySet . M.keysSet $ grove0
        entrance = fst . M.findMin $ grove0
        goal = fst . M.findMax $ grove0

        start = if forward then entrance else goal
        end = if forward then goal else entrance

findGoal :: Map Point [Tile] -> Maybe Int
findGoal = fmap fst . findPath True 0

getSnacks :: Map Point [Tile] -> Maybe _
getSnacks seed = Just . fst $ foldl' (\(n, mp) f -> fromJust $ findPath f n mp) (0, seed) [True, False, True]

day24a :: Map Point [Tile] :~> Int
day24a = MkSol
    { sParse = Just . parseAsciiMap tile
    , sShow  = show
    , sSolve = findGoal
    }

day24b :: Map Point [Tile] :~> _
day24b = MkSol
    { sParse = Just . parseAsciiMap tile
    , sShow  = show
    , sSolve = getSnacks
    }

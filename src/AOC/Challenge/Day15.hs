-- |
-- Module      : AOC.Challenge.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day15 (
    day15a
  , day15b
  ) where

import           AOC.Solver ((:~>)(..))
import           AOC.Common (parseAsciiMap, Dir(..), dirVec, Point, listTup)

import           Control.Applicative            (empty)
import           Control.Arrow                  (first)
import           Control.Monad                  (join, liftM2, guard, (<=<))
import           Data.Bifunctor                 (bimap)
import           Data.Foldable                  (foldl')
import           Data.List.Split                (splitOn)
import           Data.Map                       (Map)
import           Data.Maybe                     (fromMaybe)
import           Data.Semigroup                 (Sum(..))
import           Data.Set                       (Set)
import           Linear                         (V2(..))
import qualified Data.Map                       as M
import qualified Data.Set                       as S

data NormalWarehouse = Robot | Box | Wall deriving (Show, Eq, Ord)
data LargeWarehouse = SmallRobot | LeftBox | RightBox | SmallWall deriving (Show, Eq, Ord)

class Warehouse a where
    isRobot :: a -> Bool
    isBox :: a -> Bool

instance Warehouse NormalWarehouse where
    isRobot = (== Robot)
    isBox = (== Box)

instance Warehouse LargeWarehouse where
    isRobot = (== SmallRobot)
    isBox = (== LeftBox)

getDir :: Char -> Maybe Dir
getDir = \case
    '^' -> pure North
    'v' -> pure South
    '>' -> pure East
    '<' -> pure West
    _ -> empty

parse :: Char -> Maybe NormalWarehouse
parse '#' = pure Wall
parse '@' = pure Robot
parse 'O' = pure Box
parse _   = empty

scaleUp :: Map Point NormalWarehouse -> Map Point LargeWarehouse
scaleUp = M.fromList . concatMap go . M.toList
    where
        sf = V2 2 1
        go (k, Wall) = [(k*sf, SmallWall), (k*sf+dirVec East, SmallWall)]
        go (k, Box) = [(k*sf, LeftBox), (k*sf+dirVec East, RightBox)]
        go (k, Robot) = [(k*sf, SmallRobot)]

parseBoth :: (String, String) -> Maybe (Map Point NormalWarehouse, [Dir])
parseBoth = sequence . bimap (parseAsciiMap parse) (fmap join . traverse (traverse getDir) . lines)

simulate :: (Warehouse a) => (Dir -> Point -> Map Point a -> Maybe (Set Point)) -> Map Point a -> [Dir] -> Maybe (Map Point a)
simulate shift mp0 ds0 = do
    r <- fmap fst . M.lookupMin . M.filter isRobot $ mp0
    return $ snd $ foldl' go (r, mp0) ds0

    where
        go (r, mp) d = fromMaybe (r,mp) $ do
            ks <- shift d r mp
            let mp' = uncurry M.union
                    . first (M.fromList . map (first (+dirVec d)) . M.toList)
                    . M.partitionWithKey (\k _ -> k `S.member` ks)
                    $ mp
            return (r+dirVec d, mp')

moveRobot :: Dir -> Point -> Map Point NormalWarehouse -> Maybe (Set Point)
moveRobot d k mp = do
    let next = M.lookup (k + dirVec d) mp
    guard (Just Wall /= next)
    case next of
      Just Box -> S.insert k <$> moveRobot d (k+dirVec d) mp
      Nothing -> return $ S.singleton k
      _ -> Nothing

moveSmallRobot :: Dir -> Point -> Map Point LargeWarehouse -> Maybe (Set Point)
moveSmallRobot d k mp = do
    let (<++>) = liftM2 (<>)
    let next = M.lookup (k + dirVec d) mp
    let go' k' = moveSmallRobot d (k'+dirVec d) mp
    guard (Just SmallWall /= next)
    case (d, next) of
      (East, Just LeftBox)   -> S.insert k <$> go' k
      (East, Just RightBox)  -> S.insert k <$> go' k
      (West, Just LeftBox)   -> S.insert k <$> go' k
      (West, Just RightBox)  -> S.insert k <$> go' k
      (North, Just LeftBox)  -> S.insert k <$> go' k <++> go' (k + dirVec East)
      (South, Just LeftBox)  -> S.insert k <$> go' k <++> go' (k + dirVec East)
      (North, Just RightBox) -> S.insert k <$> go' k <++> go' (k + dirVec West)
      (South, Just RightBox) -> S.insert k <$> go' k <++> go' (k + dirVec West)
      (_, Nothing)           -> return $ S.singleton k
      _                      -> Nothing

gps :: Warehouse a => Point -> a -> Sum Int
gps (V2 x y) w
  | isBox w = pure $ 100*y + x
  | otherwise = mempty

day15 :: Warehouse a => (Map Point NormalWarehouse -> Map Point a) -> (Dir -> Point -> Map Point a -> Maybe (Set Point)) -> (Map Point a, [Dir]) :~> Int
day15 mkRoom shiftRobot = MkSol
    { sParse = fmap (first mkRoom) . parseBoth <=< listTup . splitOn "\n\n"
    , sShow  = show
    , sSolve = fmap (getSum . M.foldMapWithKey gps) . uncurry (simulate shiftRobot)
    }

day15a :: (Map Point NormalWarehouse, [Dir]) :~> Int
day15a = day15 id moveRobot

day15b :: (Map Point LargeWarehouse, [Dir]) :~> Int
day15b = day15 scaleUp moveSmallRobot

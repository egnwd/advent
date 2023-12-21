-- |
-- Module      : AOC.Challenge.Day14
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 14.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day14 (
    day14a
  , day14b
  ) where

import           AOC.Solver ((:~>)(..))
import AOC.Common (Point, Dir(..), parseAsciiMap, getEdge, boundingBox, inBoundingBox, loopEither, dirVec)

import Data.Map (Map)
import Control.Monad ((<=<))
import qualified Data.Map                       as M
import qualified Data.Set.NonEmpty              as NES
import qualified Linear                         as L

data Rock = Cube | Rolly deriving (Eq, Ord, Show)

parseRock :: Char -> Maybe Rock
parseRock '#' = Just Cube
parseRock 'O' = Just Rolly
parseRock _ = Nothing

cycleRocks :: Map Point Rock -> Map Point Rock
cycleRocks = roll East
           . roll South
           . roll West
           . roll North

roll :: Dir -> Map Point Rock -> Map Point Rock
roll d mp0 = M.foldlWithKey go (M.filter (==Cube) mp0) mp0
    where
        Just bb = boundingBox <$> (NES.nonEmptySet . M.keysSet $ mp0)
        step (k,mp) = case M.lookup (k+dirVec d) mp of
                        Just Cube -> Left k
                        Just Rolly -> Left k
                        Nothing -> if not (inBoundingBox bb (k+dirVec d))
                                      then Left k
                                      else Right (k+dirVec d,mp)
        stepBack (k,mp) = case M.lookup k mp of
                            Just Rolly -> Right (k-dirVec d, mp)
                            _ -> Left k
        go mp _ Cube = mp
        go mp k Rolly = let mnk = loopEither stepBack . (,mp) . loopEither step $ (k,mp)
                         in M.insert mnk Rolly mp

calculateLoad :: Map Point Rock -> Maybe Int
calculateLoad mp = do
    mx <- (`getEdge` South) . boundingBox <$> (NES.nonEmptySet . M.keysSet $ mp)
    return $ M.foldlWithKey (\a (L.V2 _ y) b -> case b of { Cube -> a; Rolly -> a + (1 + mx - y); }) 0 mp

firstRepeatedCount :: Ord b => [b] -> Maybe (Int, Int, b)
firstRepeatedCount = go M.empty 0
  where
      go seen t (x:xs) = case M.lookup x seen of
                           Just t' -> Just (t', t-t', x)
                           Nothing -> go (M.insert x t seen) (t+1) xs
      go _ _ []     = Nothing

toEnd :: (Int, Int, Map Point Rock) -> Map Point Rock
toEnd (n, d, x) = let i = (1000000000-n) `mod` d
                   in head . drop i . iterate cycleRocks $ x

day14a :: Map Point Rock :~> Int
day14a = MkSol
    { sParse = Just . parseAsciiMap parseRock
    , sShow  = show
    , sSolve = calculateLoad . roll North
    }

day14b :: Map Point Rock :~> Int
day14b = MkSol
    { sParse = Just . parseAsciiMap parseRock
    , sShow  = show
    , sSolve = calculateLoad . toEnd <=< firstRepeatedCount . iterate cycleRocks
    }

-- |
-- Module      : AOC.Challenge.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day13 (
    day13a
  , day13b
  ) where

import           AOC.Solver ((:~>)(..))
import AOC.Common (Point, boundingBox, parseAsciiSet)

import           Control.Applicative ((<|>))
import           Control.Lens (Lens', (^.))
import           Control.Monad   ((<=<), guard)
import           Data.Functor    (($>))
import           Data.Foldable   (asum)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import           Data.Maybe      (mapMaybe)
import           Data.Set        (Set)
import qualified Data.Map                       as M
import qualified Data.Set                       as S
import qualified Data.Set.NonEmpty              as NES
import qualified Linear                         as L

offBy :: Ord a => Set a -> Set a -> Int
offBy a b = S.size $ (a S.\\ b) `S.union` (b S.\\ a)

compareRocks :: (Int, Set Int) -> (Int, Set Int) -> Maybe (Int, Int)
compareRocks (r, rc) (r', rc')
  | 1 >= offBy rc rc' = Just (r,r')
  | otherwise = Nothing

isReflection :: Int -> Map Int (Set Int) -> [(Int, Int)] -> Bool
isReflection n mp = (n ==) . sum . mapMaybe (\(l, r) -> offBy <$> (M.lookup l mp) <*> (M.lookup r mp))

search :: Int -> Lens' Point Int -> Int -> Int -> Set Point -> Maybe Int
search n xy mn mx r0 = do
    let strips = M.mapKeysWith S.union (^. (L._yx . xy)) . M.fromSet (S.singleton . (^. xy)) $ r0
    let f (l,r) = guard (isReflection n strips (zip [l,(l-1) .. mn] [r .. mx])) $> l + 1
    asum $ zipWith (\a -> f <=< compareRocks a) (M.toList strips) (tail $ M.toList strips)

findReflection :: Int -> Set Point -> Maybe Point
findReflection n r0 = do
    (L.V2 (L.V2 xmn ymn) (L.V2 xmx  ymx)) <- boundingBox <$> NES.nonEmptySet r0
    (flip L.V2 0 <$> search n L._x ymn ymx r0) <|> (L.V2 0 <$> search n L._y xmn xmx r0)

day13 :: Int -> [Set Point] :~> Int
day13 n = MkSol
    { sParse = Just . map (parseAsciiSet (=='#')) . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . (\(L.V2 x y) -> x * 100 + y) . sum . mapMaybe (findReflection n)
    }

day13a :: [Set Point] :~> Int
day13a = day13 0

day13b :: [Set Point] :~> Int
day13b = day13 1

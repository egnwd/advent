module AOC.Common.Point
  ( Point
  , boundingBox
  , inBoundingBox
  , parseAsciiSet
  , parseAsciiMap
  , asciiGrid
  , displayAsciiMap
  , displayAsciiSet
  , neighbours
  ) where

import           Data.Monoid
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Map (Map)
import           Data.Set (Set)
import           Linear
import           Control.Lens
import           Data.Set.Lens
import           Data.Map.Lens
import           Data.Tuple.Strict
import qualified Data.Map.NonEmpty          as NEM
import qualified Data.Map                   as M

-- Some fns from: https://github.com/mstksg/advent-of-code-2020/blob/165461e51f991ac44bc9f8acc5c4e17caf83c13b/src/AOC/Common/Point.hs

type Point = V2 Int

boundingBox :: (Foldable1 f, Applicative g, Ord a) => f (g a) -> V2 (g a)
boundingBox = (\(T2 (Ap mn) (Ap mx)) -> V2 (getMin <$> mn) (getMax <$> mx))
            . foldMap1 (\p -> T2 (Ap (Min <$> p)) (Ap (Max <$> p)))

inBoundingBox
    :: (Applicative g, Foldable g, Ord a)
    => V2 (g a)
    -> g a
    -> Bool
inBoundingBox (V2 mn mx) x = and $ go <$> x <*> mn <*> mx
  where
    go x' mn' mx' = x' >= mn' && x' <= mx'

parseAsciiMap
    :: (Char -> Maybe a)
    -> String
    -> Map Point a
parseAsciiMap f = toMapOf (asciiGrid <. folding f)

parseAsciiSet
    :: (Char -> Bool)
    -> String
    -> Set Point
parseAsciiSet f = setOf (asciiGrid . filtered f . asIndex)

asciiGrid :: IndexedTraversal Point String [a] Char a
asciiGrid = conjoined traverse $ \z ->
      sequenceA
    . concat
    . zipWith (\y -> zipWith (\x -> indexed z (V2 x y :: Point)) [0..]) [0..]
    . lines

displayAsciiMap
    :: Char             -- ^ default tile
    -> Map Point Char   -- ^ tile map
    -> String
displayAsciiMap d (NEM.IsNonEmpty mp) = unlines
    [ [ NEM.findWithDefault d (V2 x y) mp
      | x <- [xMin .. xMax]
      ]
    | y <- [yMin .. yMax]
    ]
  where
    V2 xMin yMin `V2` V2 xMax yMax = boundingBox $ NEM.keysSet mp
displayAsciiMap _ _ = ""

displayAsciiSet
    :: Char      -- ^ missing tile
    -> Char      -- ^ present tile
    -> Set Point -- ^ tile set
    -> String
displayAsciiSet x y = displayAsciiMap x . M.fromSet (const y)

neighbours :: Point -> [Point]
neighbours k = [k' | k' <- (k +) <$> [ V2 0 (-1), V2 1 0, V2 0 1, V2 (-1) 0 ]]

module AOC.Common.Point
  ( Point
  , boundingBox
  , inBoundingBox
  , pastBoundingBox
  , parseAsciiSet
  , parseAsciiMap
  , asciiGrid
  , displayAsciiMap
  , displayAsciiSet
  , neighbours
  , allNeighbours
  , manhattan
  -- * D24
  , D24(..)
  , orientPoint
  , allD24
  , allD24Set
  -- * Axis
  , Axis(..)
  , allAxes
  -- * Dir
  , Dir(..)
  , dirVec
  , setEdge
  , allDir
  , allDirSet
  -- * 3D Points
  , Point3D
  , Vector3D
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Data.Foldable           (toList)
import           Data.Function
import           Data.Group
import           Data.Hashable
import           Data.List
import           Data.List.NonEmpty      (NonEmpty(..))
import           Data.Map                (Map)
import           Data.Map.Lens
import           Data.Monoid
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Set                (Set)
import           Data.Set.Lens
import           Data.Set.NonEmpty       (NESet)
import           Data.Tuple.Strict
import           GHC.Generics
import           Linear
import           Linear.Affine           (distanceA)
import qualified Data.Map                as M
import qualified Data.Map.NonEmpty       as NEM
import qualified Data.Set.NonEmpty       as NES

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

pastBoundingBox
    :: (Integral a)
    => V2 (V2 a)
    -> V2 a
    -> Bool
pastBoundingBox b p = or $ go <$> p <*> maxB
  where
    V2 xs ys = sequence b
    maxB = floor <$> maximumBy (dist @Double) [V2 (fromIntegral x) (fromIntegral y) | x <- toList xs, y <- toList ys]
    dist :: forall a. (Floating a, Ord a) => V2 a -> V2 a -> Ordering
    dist = compare `on` distanceA (V2 0 0)
    go x' mx' = if mx' < 0 then x' < mx' else x' > mx'

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
neighbours k = (k +) <$> [V2 0 (-1), V2 1 0, V2 0 1, V2 (-1) 0]

allNeighbours :: Point -> [Point]
allNeighbours k = [k + k' | k' <- sequence (pure [-1, 0, 1]), k' /= pure 0]

manhattan :: (Foldable t, Num c, Num (t c)) => t c -> t c -> c
manhattan x y = sum . abs $ x - y

-- ^ 24 3D Orientations

type Point3D = V3 Int
type Vector3D = V3 Int

data Dir = North | East | South | West
  deriving (Show, Eq, Ord, Generic, Enum)

dirVec :: Num a => Dir -> V2 a
dirVec = \case
    North -> V2   0 (-1)
    East  -> V2   1   0
    South -> V2   0   1
    West  -> V2 (-1)  0

getEdge :: V2 (V2 a)
        -> Dir
        -> a
getEdge (V2 (V2 w n) (V2 e s))
  = \case
    North -> n
    East  -> e
    South -> s
    West  -> w

setEdge :: V2 (V2 a) -> V2 a -> Dir -> V2 a
setEdge bounds (V2 i j) dir
  = let e = getEdge bounds dir
     in case dir of
          North -> V2 i e
          East  -> V2 e j
          South -> V2 i e
          West  -> V2 e j

instance Hashable Dir
instance NFData Dir

mulDir :: Dir -> Dir -> Dir
mulDir North = id
mulDir East  = \case North -> East
                     East  -> South
                     South -> West
                     West  -> North
mulDir South = \case North -> South
                     East  -> West
                     South -> North
                     West  -> East
mulDir West  = \case North -> West
                     East  -> North
                     South -> East
                     West  -> South

allDir :: NonEmpty Dir
allDir = North :| [ East .. ]

allDirSet :: NESet Dir
allDirSet = NES.fromDistinctAscList allDir

instance Semigroup Dir where
    (<>) = mulDir
    stimes n x = case n `mod` 4 of
      1 -> x
      2 -> x <> x
      3 -> invert x
      _ -> North

instance Monoid Dir where
    mempty = North

instance Group Dir where
    invert = \case North -> North
                   East  -> West
                   South -> South
                   West  -> East
    pow = flip stimes

data Axis = ZAxis | YAxis | XAxis
  deriving (Show, Eq, Ord, Generic, Enum)

instance Hashable Axis
instance NFData Axis

allAxes :: NonEmpty Axis
allAxes = XAxis :| [ YAxis, ZAxis ]

data D24 = D24 { d24Rot :: !Dir, d24Axis :: !Axis, d24Flip :: !Bool }
  deriving (Show, Eq, Ord, Generic)

instance Hashable D24
instance NFData D24

allD24 :: NonEmpty D24
allD24 = D24 <$> allDir <*> allAxes <*> (False :| [ True ])

allD24Set :: NESet D24
allD24Set = NES.fromDistinctAscList allD24

orientPoint :: Num a => D24 -> V3 a -> V3 a
orientPoint = \case
    D24 North XAxis False -> \(V3 x y z) -> V3   z     y (-x)
    D24 East  XAxis False -> \(V3 x y z) -> V3   z  (-x) (-y)
    D24 West  XAxis False -> \(V3 x y z) -> V3   z    x    y
    D24 South XAxis False -> \(V3 x y z) -> V3   z  (-y)   x

    D24 North XAxis True  -> \(V3 x y z) -> V3 (-z)   y    x
    D24 East  XAxis True  -> \(V3 x y z) -> V3 (-z) (-x)   y
    D24 West  XAxis True  -> \(V3 x y z) -> V3 (-z)   x  (-y)
    D24 South XAxis True  -> \(V3 x y z) -> V3 (-z) (-y) (-x)

    D24 North YAxis False -> \(V3 x y z) -> V3   x  (-z)   y
    D24 East  YAxis False -> \(V3 x y z) -> V3 (-y) (-z)   x
    D24 West  YAxis False -> \(V3 x y z) -> V3   y  (-z) (-x)
    D24 South YAxis False -> \(V3 x y z) -> V3 (-x) (-z) (-y)

    D24 North YAxis True  -> \(V3 x y z) -> V3   x    z  (-y)
    D24 East  YAxis True  -> \(V3 x y z) -> V3   y    z    x
    D24 West  YAxis True  -> \(V3 x y z) -> V3 (-y)   z  (-x)
    D24 South YAxis True  -> \(V3 x y z) -> V3 (-x)   z    y

    D24 North ZAxis False -> \(V3 x y z) -> V3   x    y    z
    D24 East  ZAxis False -> \(V3 x y z) -> V3   y  (-x)   z
    D24 West  ZAxis False -> \(V3 x y z) -> V3 (-y)   x    z
    D24 South ZAxis False -> \(V3 x y z) -> V3 (-x) (-y)   z

    D24 North ZAxis True  -> \(V3 x y z) -> V3 (-x)   y  (-z)
    D24 East  ZAxis True  -> \(V3 x y z) -> V3   y    x  (-z)
    D24 West  ZAxis True  -> \(V3 x y z) -> V3 (-y) (-x) (-z)
    D24 South ZAxis True  -> \(V3 x y z) -> V3   x  (-y) (-z)


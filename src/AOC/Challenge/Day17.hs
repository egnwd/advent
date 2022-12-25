-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable

module AOC.Challenge.Day17 (
    day17a
  , day17b
  ) where

import           AOC.Solver ((:~>)(..))
import           AOC.Common (Dir(..), Point, dirVec, boundingBox, loopEither)
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Function (on)
import           Data.List (find)
import           Data.Set (Set)
import           Data.Map (Map)
import           Linear (V2(..), _x, _y)
import           Control.Lens (folded, maximumOf, (^.), view, (+~), over, _1)
import           Data.Finite (getFinite)

import qualified Data.Map                       as M
import qualified Data.Set                       as S
import qualified Data.Set.NonEmpty              as NES
import qualified Data.Vector.Unboxed.Sized      as VS

rock1, rock2, rock3, rock4, rock5 :: Set Point

-- ####
rock1 = S.fromList
    [ V2 0 0
    , V2 1 0
    , V2 2 0
    , V2 3 0
    ]

-- .#.
-- ###
-- .#.
rock2 = S.fromList
    [ V2 0 1
    , V2 1 0
    , V2 1 1
    , V2 1 2
    , V2 2 1
    ]

-- ..#
-- ..#
-- ###
rock3 = S.fromList
    [ V2 0 0
    , V2 1 0
    , V2 2 0
    , V2 2 1
    , V2 2 2
    ]

-- #
-- #
-- #
-- #
rock4 = S.fromList
    [ V2 0 0
    , V2 0 1
    , V2 0 2
    , V2 0 3
    ]

-- ##
-- ##
rock5 = S.fromList
    [ V2 0 0
    , V2 0 1
    , V2 1 0
    , V2 1 1
    ]

allRocks :: [Set Point]
allRocks = [rock1, rock2, rock3, rock4, rock5]

theFloor :: Set Point
theFloor = S.fromList
    [ V2 0 0
    , V2 1 0
    , V2 2 0
    , V2 3 0
    , V2 4 0
    , V2 5 0
    , V2 6 0
    ]

type FingerPrint = VS.Vector 7 Int

fingerprint :: Set Point -> FingerPrint
fingerprint vent = VS.generate ((subtract bottom) . (cols M.!) . fromIntegral . getFinite)
    where
        cols :: Map Int Int
        cols = M.fromListWith max . map (\(V2 c r) -> (c,r)) . S.toList $ vent
        bottom = minimum cols

congruent :: Set Point -> Set Point -> Bool
congruent = (==) `on` fingerprint

heightOfTower :: Int -> [Dir] -> Maybe Int
heightOfTower nRocks p = do
    let jets = cycle p
    let rocks = cycle allRocks
    let start = (theFloor, jets, rocks)

    let next f = tail . iterate f

    let tortoise = next dropRock start
    let hare = next (dropRock . dropRock) start

    let getRepeatedState a b n = (congruent (a ^. _1) (b ^. _1), a, b, n)

    (_, (ventPattern, jets', rocks'), (ventPattern', _, _), n) <- find (view _1) $ zipWith3 getRepeatedState tortoise hare [1..]

    top <- maximumOf (folded . _y) ventPattern
    top' <- maximumOf (folded . _y) ventPattern'
    return (top, top')

    let (q, r) = nRocks `divMod` n
    h <- maximumOf (_1 . folded . _y) . head . drop r $ iterate dropRock (ventPattern, jets', rocks')
    return $ top + (top'-top) * (q-1) + (h - top)

dropRock :: (Set Point, [Dir], [Set Point]) -> (Set Point, [Dir], [Set Point])
dropRock (vent, js, (r:rs)) = (vent', js', rs)
    where
        height = fromMaybe 0 $ maximumOf (folded . _y) vent
        start = V2 2 (height + 4)
        shiftRock :: V2 Int -> Set Point -> Set Point
        shiftRock (V2 x y) r = let r' = S.map ((_y +~ y) . (_x +~ x)) r :: Set Point
                                   (V2 (V2 mnx _) (V2 mxx _)) = boundingBox (fromJust . NES.nonEmptySet $ r')
                                in if vent `S.disjoint` r' && mnx >= 0 && mxx < 7 then r' else r
        (restingPlace, js') = loopEither moveRock (shiftRock start r, js)
        vent' = vent `S.union` restingPlace
        moveRock (r, j:js) = let blown = shiftRock (dirVec j) r
                                 down = shiftRock (dirVec North) blown
                              in if down == blown then Left (down, js)
                                                  else Right (down, js)

toDir :: Char -> Maybe Dir
toDir '<' = Just West
toDir '>' = Just East
toDir _   = Nothing

day17 :: Int -> [Dir] :~> Int
day17 n = MkSol
    { sParse = traverse toDir
    , sShow = show
    , sSolve = heightOfTower n
    }

day17a :: [Dir] :~> Int
day17a = day17 2022

day17b :: [Dir] :~> Int
day17b = day17 (10^12)

{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day17 (
    day17a
  , day17b
  ) where

import           AOC.Prelude
import Linear hiding (trace, transpose)
import Control.Lens

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
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

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

allRocks = [rock1, rock2, rock3, rock4, rock5]

theFloor = S.fromList
    [ V2 0 0
    , V2 1 0
    , V2 2 0
    , V2 3 0
    , V2 4 0
    , V2 5 0
    , V2 6 0
    ]

lcp :: Eq a => [[a]] -> [a]
lcp = fmap head . takeWhile ((all . (==) . head) <*> tail) . transpose

isRepeated :: Set Point -> Bool
isRepeated xs = fromMaybe False isRepeated'
    where
        isRepeated' = do
            let noFloor = S.filter ((/=0) . view _y) xs
            mid <- (`div` 2) <$> (maximumOf (folded . _y) noFloor)
            let rows = M.fromListWith (++) . map (\(V2 x y) -> (y, [x])) . S.toAscList $ noFloor
            let (M.elems->below, M.elems->above) = M.split mid rows
            return $ (length below == length above) && (not . null . lcp $ [below, above])


heightOfTower' :: Int -> _ -> _
heightOfTower' n p = head . filter isRepeated . toListOf (traverse . _1) $ iterate dropRock (theFloor, jets, rocks)
    where
        jets = cycle p
        rocks = cycle $ allRocks
        dropRock :: (Set Point, [_], [Set Point]) -> (Set Point, [_], [Set Point])
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

heightOfTower :: Int -> _ -> _
heightOfTower n p = maximumOf (_1) . head . drop n $ iterate dropRock (theFloor, jets, rocks)
    where
        jets = cycle p
        rocks = cycle $ allRocks
        dropRock :: (Set Point, [_], [Set Point]) -> (Set Point, [_], [Set Point])
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

toDir '<' = Just West
toDir '>' = Just East
toDir _   = Nothing

showVent = ('\n' :) . displayAsciiSet '.' '#' . S.map (over _y negate)

day17a :: _ :~> _
day17a = MkSol
    { sParse = traverse toDir
    , sShow  = show
    , sSolve = heightOfTower (dyno_ "rocks" 2022)
    }

day17b :: _ :~> _
day17b = MkSol
    { sParse = traverse toDir
    , sShow  = showVent
    , sSolve = Just . heightOfTower' (dyno_ "rocks" 1000000000000)
    }

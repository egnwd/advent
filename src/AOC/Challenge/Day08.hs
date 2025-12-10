{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day08 (
    day08a
  , day08b
  ) where

import           AOC.Prelude

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
import Linear
import Control.Lens

listToV3 :: [String] -> Maybe Point3D
listToV3 [x,y,z] = V3 <$> readMaybe x <*> readMaybe y <*> readMaybe z
listToV3 _ = Nothing

pointsToGraph :: [Point3D] -> PSQ.OrdPSQ (Point3D, Point3D) Double (Set Point3D)
pointsToGraph xs = PSQ.fromList . filter ((/=0) . view _2) $ [((x,y), d x y, S.fromList [x, y]) | (x:ys) <- tails xs, y <- ys]
    where
        d :: Point3D -> Point3D -> Double
        d a b = distance (fromIntegral <$> a) (fromIntegral <$> b)

buildNStepsGraph :: Int -> PSQ.OrdPSQ (Point3D, Point3D) Double (Set Point3D) -> [Map Point3D (Set Point3D)]
buildNStepsGraph n0 ns0 = buildGraph' n0 ns0 []
    where
        buildGraph' 0 _ visited = visited
        buildGraph' n ns visited = case PSQ.minView ns of
            Nothing -> visited
            Just (_, _, toAdd, ns') ->
                let visited' = insertPoints toAdd visited
                 in buildGraph' (n-1) ns' visited'

buildGraph :: PSQ.OrdPSQ (Point3D, Point3D) Double (Set Point3D) -> Maybe (Point3D, Point3D)
buildGraph ns0 = buildGraph' Nothing ns0 seed
    where
        seed = map (`M.singleton` S.empty) . S.toList . foldMapByOf (traverse . both) (<>) S.empty S.singleton . PSQ.keys $ ns0
        buildGraph' k ns visited
          | length visited == 1 = k
          | otherwise = case PSQ.minView ns of
            Nothing -> k
            Just (k', _, toAdd, ns') ->
                let visited' = insertPoints toAdd visited
                 in buildGraph' (Just k') ns' visited'

insertPoints :: Set Point3D -> [Map Point3D (Set Point3D)] -> [Map Point3D (Set Point3D)]
insertPoints newSet visited
  | any (S.isSubsetOf newSet . M.keysSet) visited = visited
  | otherwise = let (inN, outN) = partition (not . S.disjoint newSet . M.keysSet) visited
                    addPoint k = maybe (S.delete k newSet) (S.union (S.delete k newSet))
                    toAdd = foldr (\k -> M.alter (Just . addPoint k) k) (M.unions inN) $ S.toList newSet
                 in toAdd : outN

getTop3 :: [Map Point3D (Set Point3D)] -> Int
getTop3 = product . take 3 . sortOn negate . map M.size

day08a :: _ :~> _
day08a = MkSol
    { sParse = traverse (listToV3 . splitOn ",") . lines
    , sShow  = show
    , sSolve = Just . getTop3 . buildNStepsGraph (dyno_ "n" 1000) . pointsToGraph
    }

day08b :: _ :~> _
day08b = MkSol
    { sParse = sParse day08a
    , sShow  = show
    , sSolve = fmap (foldMapByOf (both . _x) (*) 1 id) . buildGraph . pointsToGraph
    }

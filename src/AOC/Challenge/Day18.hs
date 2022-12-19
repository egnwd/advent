{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day18
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 18.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day18 (
    day18a
  , day18b
  ) where

import           AOC.Prelude

import Control.Lens
import Linear
import Data.Ix

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
import qualified Data.Set.NonEmpty             as NES
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

type Side = V2 Point3D

offsets :: [Side]
offsets =
    [ V2 (V3 0 0 0) (V3 1 1 0) -- front
    , V2 (V3 0 0 1) (V3 1 1 1) -- back
    , V2 (V3 1 0 0) (V3 1 1 1) -- right
    , V2 (V3 0 0 0) (V3 0 1 1) -- left
    , V2 (V3 0 1 0) (V3 1 1 1) -- top
    , V2 (V3 0 0 0) (V3 1 0 1) -- bottom
    ]

sides :: Point3D -> Set Side
sides p = S.fromList $ (fmap (+p)) <$> offsets

findSides :: (Foldable t) => t Point3D -> Maybe (NES.NESet Side)
findSides = NES.nonEmptySet . foldl' (\ss s -> (ss `S.union` s) S.\\ (ss `S.intersection` s)) S.empty . map sides . toList

connected :: Point3D -> Set Point3D -> Bool
connected x = not . S.null . S.intersection (neighboursSet x)

findConnected :: Set Point3D -> V2 Point3D -> Set Point3D -> Set Point3D -> Set Point3D
findConnected cubes bb r0 xs0 = fixedPoint (findConnected' xs0) r0
    where
        findConnected' :: Set Point3D -> Set Point3D -> Set Point3D
        findConnected' xs r = S.foldl' go r $ xs S.\\ r
        go a x
          | inBoundingBox bb x && x `S.notMember` cubes && x `connected` a = S.insert x a
          | otherwise = a

findExteriorSides :: [Point3D] -> _
findExteriorSides ps = do
    let cubes = S.fromList ps
    ss <- findSides ps
    let relax = (_x %~ (subtract 1)) . (_y %~ (+1))
    bb@(V2 mn mx) <- (relax . boundingBox . NES.fromList) <$> (NE.nonEmpty . toListOf (folded . each) $ ss)
    let (z:rs) = range (mn, mx)
    let reachableCubes = findConnected cubes bb (S.singleton z) (S.fromList rs)
    reachableSides <- findSides reachableCubes
    NES.nonEmptySet $ reachableSides `NES.intersection` ss

day18a :: _ :~> _
day18a = MkSol
    { sParse = traverse parseLine . lines
    , sShow  = show
    , sSolve = fmap NES.size . findSides
    }

day18b :: _ :~> _
day18b = MkSol
    { sParse = traverse parseLine . lines
    , sShow  = show
    , sSolve = fmap NES.size . findExteriorSides
    }

parseLine :: String -> Maybe Point3D
parseLine l = do
    [x,y,z] <- traverse readMaybe . splitOn "," $ l
    return $ V3 x y z

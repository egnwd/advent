{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day06 (
    day06a
  , day06b
  ) where

import           AOC.Prelude
import Linear
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S

parseCoords = V2 <$> pDecimal <* pTok "," <*> pDecimal

solvea :: NE.NonEmpty Point -> _
solvea cs = maximum . freqs . M.elems $ M.filter (`S.notMember` infinite) closest
    where
        infinite = S.fromList . M.elems $ M.restrictKeys closest edges
        closest = M.mapMaybe dists . M.fromList $ zip box box
        dists :: Point -> Maybe Point
        dists c = let mn = getMin $ foldMap (Min . manhattan c) cs
                   in case filter ((== mn) . manhattan c) (NE.toList cs) of
                        [x] -> Just x
                        _ -> Nothing
        box = spaces cs
        (V2 (V2 mnx mny) (V2 mxx mxy)) = boundingBox cs
        edges = S.fromList $ concatMap sequence [V2 [mnx..mxx] [mny], V2 [mnx] [mny..mxy], V2 [mnx..mxx] [mxy], V2 [mxx] [mny..mxy]]


spaces = sequence . (\(V2 (V2 mnx mny) (V2 mxx mxy)) -> V2 [mnx..mxx] [mny..mxy]) . boundingBox

solveb :: Int -> NE.NonEmpty Point -> _
solveb limit cs = countTrue ((< limit) . totalDist) . spaces $ cs
    where
        totalDist c = sum $ (manhattan c) <$> cs

day06a :: _ :~> _
day06a = MkSol
    { sParse = fmap NE.fromList . parseLines parseCoords
    , sShow  = show
    , sSolve = Just . solvea
    }

day06b :: _ :~> _
day06b = MkSol
    { sParse = fmap NE.fromList . parseLines parseCoords
    , sShow  = show
    , sSolve = Just . solveb (dyno_ "limit" 10000)
    }

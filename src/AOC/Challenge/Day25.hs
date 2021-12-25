{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day25
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 25.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day25 (
    day25a
  , day25b
  ) where

import           AOC.Prelude
import Linear
import Control.Arrow
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE

parseDir '>' = Just East
parseDir 'v' = Just South
parseDir _   = Nothing

idxFixedPoint :: Eq a => (a -> a) -> a -> (Int, a)
idxFixedPoint f = go 1
  where
    go idx !x
        | x == y    = (idx, x)
        | otherwise = go (idx+1) y
      where
        y = f x

solve mp = fst . idxFixedPoint (step (boundingBox (NE.fromList . M.keys $ mp))) $ mp

step :: V2 Point -> Map Point Dir -> Map Point Dir
step bounds = stepDir South . stepDir East
    where
        stepDir :: Dir -> Map Point Dir -> Map Point Dir
        stepDir dir mp = M.mapKeys moveCuke mp
            where
                dirs = M.keysSet . M.filter (==dir) $ mp
                v :: Point
                v = dirVec dir
                nextP :: Point -> Point
                nextP p | inBoundingBox bounds (p + v) = p + v
                        | otherwise = setEdge bounds (p + v) (dir <> South)
                moveCuke :: Point -> Point
                moveCuke p = let p' = nextP p in if p `S.member` dirs && p' `M.notMember` mp then p' else p

day25a :: _ :~> _
day25a = MkSol
    { sParse = Just . parseAsciiMap parseDir
    , sShow  = show
    , sSolve = Just . solve
    }

day25b :: _ :~> _
day25b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

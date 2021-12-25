-- |
-- Module      : AOC.Challenge.Day25
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 25.

module AOC.Challenge.Day25 (
    day25a
  , day25b
  ) where

import           AOC.Solver ((:~>)(..))
import           AOC.Common (indexedFixedPoint, parseAsciiMap, Dir(..), Point, dirVec)
import           Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as M
import qualified Data.Set.NonEmpty as S

parseDir :: Char -> Maybe Dir
parseDir '>' = Just East
parseDir 'v' = Just South
parseDir _   = Nothing

solve :: NEMap Point Dir -> Int
solve mp = fst . indexedFixedPoint (step mx) $ mp
    where
        (mx, _)  = M.findMax mp

step :: Point -> NEMap Point Dir -> NEMap Point Dir
step bounds = stepDir South . stepDir East
    where
        stepDir :: Dir -> NEMap Point Dir -> NEMap Point Dir
        stepDir dir mp = M.mapKeys moveCuke mp
            where
                dirs = fmap M.keysSet . M.nonEmptyMap . M.filter (==dir) $ mp
                moveCuke p = let p' = mod <$> p + dirVec dir <*> bounds
                                 isDir = maybe False (S.member p) dirs
                              in if isDir && p' `M.notMember` mp then p' else p

day25a :: NEMap Point Dir :~> Int
day25a = MkSol
    { sParse = M.nonEmptyMap . parseAsciiMap parseDir
    , sShow  = show
    , sSolve = Just . solve
    }

day25b :: String :~> String
day25b = MkSol
    { sParse = Just
    , sShow  = id
    , sSolve = Just . const "Christmas saved!"
    }

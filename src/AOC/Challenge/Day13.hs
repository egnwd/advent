{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day13 (
    day13a
  , day13b
  ) where

import           AOC.Prelude
import AOC.Common.Point
import Control.Arrow ((&&&))
import qualified Data.Map as M
import qualified Data.Set as S

data Intersection = I { exits :: Set Dir } deriving Show

parseIntersection x = x <$ (guard (x /= ' '))

toCar = \case
 '>' -> pure East
 '<' -> pure West
 '^' -> pure North
 'v' -> pure South
 _ -> Nothing

justCars = M.mapMaybe toCar

justIntersections :: Map Point Char -> Map Point _
justIntersections tracks = tracks'
    where
        tracks' = M.mapMaybe id . M.mapWithKey toIntersection $ tracks
        toIntersection loc c = do
            guard (c /= ' ')
            let es = M.fromList . mapMaybe (sequence . second (flip M.lookup tracks . (+loc) . dirVec)) $ zip [North ..] [North ..]
            case (c, M.lookup North es) of
              ('/', Just '|')  -> return $ I (S.fromList [North, West])
              ('/', _)         -> return $ I (S.fromList [South, East])
              ('\\', Just '|') -> return $ I (S.fromList [North, East])
              ('\\', _)        -> return $ I (S.fromList [South, West])
              ('+', _)         -> return $ I (S.fromList [South, West, North, East])
              (_, _)           -> Nothing


day13a :: _ :~> _
day13a = MkSol
    { sParse = Just . (justIntersections &&& justCars) . parseAsciiMap parseIntersection
    , sShow  = show
    , sSolve = Just
    }

day13b :: _ :~> _
day13b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

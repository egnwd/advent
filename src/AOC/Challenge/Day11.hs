{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day11 (
    day11a
  , day11b
  ) where

import           AOC.Prelude
import Linear
import Control.Lens
import Control.DeepSeq
import qualified Data.Map as M

solvea ps = map (\f -> (f, getTotalPower f)) . sequence $ V2 [1..298] [1..298]
    where
        getTotalPower f = getSum . foldMap (Sum . (ps M.!) . (+f)) . sequence $ V2 [0..2] [0..2]

powers serial = M.fromList . map (\f -> (f, powerLevel serial f)) . sequence $ V2 [1..300] [1..300]

powerLevel serial (V2 x y) = let rackId = (x + 10)
                                 baseNumber = ((rackId * y) + serial) * rackId
                              in ((baseNumber `div` 100) `mod` 10) - 5

summableAreaTable :: Map Point Int -> Map Point Int
summableAreaTable m = force sat
    where
        sat = M.mapWithKey go m
        go k v = sum $ catMaybes
            [ Just v
            , M.lookup (k - (V2 1 0)) sat
            , M.lookup (k - (V2 0 1)) sat
            , negate <$> M.lookup (k - (V2 1 1)) sat
            ]

total :: Map Point Int -> Int -> Point -> Int
total m s p = sum $ catMaybes [ M.lookup (p + (V2 s     s) - 1) m
                              , M.lookup (p                - 1) m
                              , negate <$> M.lookup (p + (V2 s 0) - 1) m
                              , negate <$> M.lookup (p + (V2 0 s) - 1) m
                              ]

day11a :: Int :~> _
day11a = MkSol
    { sParse = readMaybe
    , sShow  = \(V2 x y) -> show x ++ "," ++ show y
    , sSolve = fmap fst . maximumByOf traverse (compare `on` snd) . solvea . powers
    }

day11b :: Int :~> _
day11b = MkSol
    { sParse = readMaybe
    , sShow  = \(V2 x y, s) -> show x ++ "," ++ show y ++ "," ++ show s
    , sSolve = \serial -> let sat = summableAreaTable . powers $ serial
                           in fmap fst . maximumByOf (traverse.traverse) (compare `on` snd)
                               . map (\s -> map (\p -> ((p,s), total sat s p)) . sequence $ V2 [1..300-s+1] [1..300-s+1]) $ [1..300]
    }

{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day10 (
    day10a
  , day10b
  ) where

import           AOC.Prelude
import           Linear
import qualified Data.Set as S

angleTo :: Point -> Point -> Float
angleTo p q = atan (fromIntegral dy / fromIntegral dx)
    where
        V2 dy dx = p - q

inSight :: Set Point -> Point -> [Point]
inSight as p = filter go . S.toList . S.delete p $ as
    where
        go q = all (`S.notMember` as) . tail . init $ lineTo (V2 p q)

solvea as = getMax . foldMap (Max . length . inSight as) $ as

findWinningBet as l = V2 8 2

day10a :: _ :~> _
day10a = MkSol
    { sParse = Just . parseAsciiSet (== '#')
    , sShow  = show
    , sSolve = Just . solvea
    }

day10b :: _ :~> _
day10b = MkSol
    { sParse = Just . parseAsciiSet (=='#')
    , sShow  = \(V2 x y) -> show $ x * 100 + y
    , sSolve = \as -> Just . findWinningBet as . solvea $ as
    }

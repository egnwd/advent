{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day04 (
    day04a
  , day04b
  ) where

import           AOC.Prelude
import Data.Maybe
import Control.Lens

twoConsecutiveNumbers :: String -> Maybe String
twoConsecutiveNumbers p = go p
    where
        go :: String -> Maybe String
        go [] = Nothing
        go [_] = Nothing
        go (a:b:bs)
          | a == b = Just p
          | otherwise = go (b:bs)

monotonicIncrease :: String -> Maybe String
monotonicIncrease p = go p
    where
        go :: String -> Maybe String
        go [] = Just p
        go [_] = Just p
        go (a:b:bs)
            | digitToInt a <= digitToInt b = go (b:bs)
            | otherwise = Nothing

alonePair p | any (==2) . map length . group $ p = Just p
            | otherwise = Nothing

combineRules :: [String -> Maybe String] -> String -> Maybe String
combineRules rs i = foldM (&) i rs

solvea mn mx = countTrue (isJust . passwordRule . show) [mn..mx]
    where
        rules = [twoConsecutiveNumbers, monotonicIncrease]
        passwordRule = combineRules rules

solveb mn mx = countTrue (isJust . passwordRule . show) [mn..mx]
    where
        rules = [alonePair, monotonicIncrease]
        passwordRule = combineRules rules

day04a :: _ :~> _
day04a = MkSol
    { sParse = traverseOf both (readMaybe @ Int . filter isDigit) . splitAt 6
    , sShow  = show
    , sSolve = Just . uncurry solvea
    }

day04b :: _ :~> _
day04b = MkSol
    { sParse = traverseOf both (readMaybe @ Int . filter isDigit) . splitAt 6
    , sShow  = show
    , sSolve = Just . uncurry solveb
    }

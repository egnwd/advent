{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day05 (
    day05a
  , day05b
  ) where

import           AOC.Prelude

reduce a (b:x)
  | toUpper a == toUpper b && a /= b = x
  | otherwise = a:b:x
reduce c [] = [c]

options :: String -> [String]
options x = do
    m <- ['a'..'z']
    return $ filter ((/= m) . toLower) x

day05a :: _ :~> _
day05a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just . length . foldr reduce []
    }

day05b :: _ :~> _
day05b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just . getMin . foldMap (Min . length . foldr reduce []) . options . foldr reduce []
    }

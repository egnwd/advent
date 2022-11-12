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

import           Advent.OCR
import           AOC.Prelude     hiding (First, getFirst)
import           Data.List.Split
import           Data.Monoid
import qualified Data.Set        as S

toPixel :: Char -> Maybe Char
toPixel '0' = Just ' '
toPixel '1' = Just '#'
toPixel _   = Nothing

day08a :: _ :~> _
day08a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
        . snd . getMin
        . foldMap (\l -> let fs = freqs l in Min (lookupFreq '0' fs, lookupFreq '1' fs * lookupFreq '2' fs))
        . chunksOf (dyno_ "w" 25 * dyno_ "h" 6)
    }

day08b :: _ :~> _
day08b = MkSol
    { sParse = Just
    , sShow  = fromMaybe "" . asciiMapToLetters (S.fromList "#") . unlines . chunksOf 25
    , sSolve = traverse (getFirst . foldMap (First . toPixel)) . transpose . chunksOf (dyno_ "w" 25 * dyno_ "h" 6)
    }

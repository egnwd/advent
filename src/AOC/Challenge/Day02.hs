{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import           AOC.Prelude
import qualified Data.IntMap as IM
import qualified Data.Map as M

checksum ws = (*) <$> M.lookup 2 ws <*> M.lookup 3 ws

findBox ws = catMaybes <$> find (\m -> 1 == (countTrue isNothing m)) masks
    where
        masks = mask <$> ws <*> ws
        mask = zipWith (\a b -> a <$ guard (a == b))

day02a :: _ :~> _
day02a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = checksum . freqs . concatMap (IM.keys . revFreq)
    }

day02b :: _ :~> _
day02b = MkSol
    { sParse = Just . lines
    , sShow  = id
    , sSolve = findBox
    }

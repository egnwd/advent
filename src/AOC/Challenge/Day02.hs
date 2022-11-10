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
import AOC.Common.Intcode
import AOC.Common.Intcode.Memory
import Control.Lens
import qualified Data.Map as M
import Data.List.Split (splitOn)

solvea :: Int -> Int -> Memory -> Maybe Int
solvea p1 p2 = M.lookup 0 . mRegs <=< stepTilTermination . (_mRegs %~ M.insert 1 p1 . M.insert 2 p2)

solveb m = do
    let goal = 19690720
    zero <- solvea 0 0 m
    one <- solvea 1 0 m
    let change = one - zero
    let (noun, verb) = (goal - zero) `divMod` change
    return $ 100 * noun + verb

day02a :: Memory :~> _
day02a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = solvea (dyno_ "pos1" 12) (dyno_ "pos2" 2)
    }

day02b :: _ :~> _
day02b = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = solveb
    }

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

import AOC.Prelude hiding (Down)

data Direction = Forward | Up | Down | Backward deriving Eq

type Instructions = [(Direction, Int)]

parser :: CharParser Instructions
parser = undefined

parserDirty :: [String] -> (Direction, Int)
parserDirty ["forward", n] = (Forward, read n)
parserDirty ["up", n] = (Up, read n)
parserDirty ["backward", n] = (Backward, read n)
parserDirty ["down", n] = (Down, read n)
parserDirty _ = error "Oh no"

solve :: Instructions -> (Int, Int) -> (Int, Int)
solve [] p = p
solve ((Forward, n):rs) (x,y) = solve rs (x-n, y)
solve ((Backward, n):rs) (x,y) = solve rs (x+n, y)
solve ((Up, n):rs) (x,y) = solve rs (x, y+n)
solve ((Down, n):rs) (x,y) = solve rs (x, y-n)

solveb :: Instructions -> (Int, Int, Int) -> (Int, Int, Int)
solveb [] p = p
solveb ((Forward, n):rs) (x,y, a) = solveb rs (x+n, y+(a*n), a)
solveb ((Backward, n):rs) (x,y, a) = solveb rs (x-n, y, a)
solveb ((Up, n):rs) (x,y, a) = solveb rs (x, y, a-n)
solveb ((Down, n):rs) (x,y,a) = solveb rs (x, y, a+n)

day02a :: Instructions :~> _
day02a = MkSol
    { sParse = Just . map (parserDirty . words) . lines
    , sShow  = show
    , sSolve = \x -> let (z,y) = solve x (0,0) in Just (z*y)
    }

day02b :: _ :~> _
day02b = MkSol
    { sParse = sParse day02a
    , sShow  = show
    , sSolve = \x -> let (z,y,a) = solveb x (0,0,0) in Just (z*y)
    }

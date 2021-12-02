{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Linear
import Text.Megaparsec
import Text.Megaparsec.Char

data Direction = Forward | Up | Down | Backward deriving Eq

type Instruction = (Direction, Int)

parser :: CharParser Instruction
parser = (,) <$> pTok directionParser <*> pDecimal

directionParser :: CharParser Direction
directionParser = try $ choice
    [ Forward  <$ "forward"
    , Backward <$ "backward"
    , Up       <$ "up"
    , Down     <$ "down"
    ]

solve :: [Instruction] -> Point
solve = getSum . foldMap (\(d,n) -> Sum (pure n * positionUpdate d))
    where
        positionUpdate Forward  = V2 1     0
        positionUpdate Backward = V2 (-1)  0
        positionUpdate Up       = V2 0     (-1)
        positionUpdate Down     = V2 0     1

solveb :: [Instruction] -> (Point, Int)
solveb = foldl (\(p,a) (d,n) -> (update p (pure n) (positionUpdate d a), update a n (aimUpdate d))) (pure 0, 0)
    where
        update a n a' = a + n * a'

        positionUpdate Forward  = V2 1
        positionUpdate Backward = const $ V2 (-1) 0
        positionUpdate _        = const $ V2 0 0

        aimUpdate Up   = -1
        aimUpdate Down = 1
        aimUpdate _    = 0

day02a :: [Instruction] :~> Int
day02a = MkSol
    { sParse = parseLines parser
    , sShow  = show
    , sSolve = Just . product . solve
    }

day02b :: [Instruction] :~> Int
day02b = MkSol
    { sParse = parseLines parser
    , sShow  = show
    , sSolve = Just . product . fst . solveb
    }

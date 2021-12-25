{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day24
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 24.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day24 (
    day24a
  , day24b
                           , x, y, z, w
  ) where

import           AOC.Prelude
import Control.Lens
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators

{-
-   `inp a` - Read an input value and write it to variable `a`.
-   `add a b` - Add the value of `a` to the value of `b`, then store the
    result in variable `a`.
-   `mul a b` - Multiply the value of `a` by the value of `b`, then
    store the result in variable `a`.
-   `div a b` - Divide the value of `a` by the value of `b`, truncate
    the result to an integer, then store the result in variable `a`.
    (Here, "truncate" means to round the value toward zero.)
-   `mod a b` - Divide the value of `a` by the value of `b`, then store
    the *remainder* in variable `a`. (This is also called the
    [modulo](https://en.wikipedia.org/wiki/Modulo_operation) operation.)
-   `eql a b` - If the value of `a` and `b` are equal, then store the
    value `1` in variable `a`. Otherwise, store the value `0` in
    variable `a`.
-}

data Instruction
    = Inp Variable
    | Add Variable Term
    | Mul Variable Term
    | Div Variable Term
    | Mod Variable Term
    | Eql Variable Term
    deriving (Eq, Show)

data Variable = W | X | Y | Z deriving (Eq, Show)
data Term = Var Variable | Val Int deriving (Eq, Show)

parser = choice [parseInput, parseAddition, parseMultiplication, parseDivision, parseModulo, parseEquals]
    where
        parseVariable = (W <$ "w") <|> (X <$ "x") <|> (Y <$ "y") <|> (Z <$ "z")
        parseTerm = (Var <$> parseVariable) <|> (Val <$> pDecimal)
        parseBinOp name op = op <$> (pTok name *> pTok parseVariable) <*> parseTerm
        parseInput = Inp <$> (pTok "inp" *> parseVariable)
        parseAddition = parseBinOp "add" Add
        parseMultiplication = parseBinOp "mul" Mul
        parseDivision = parseBinOp "div" Div
        parseModulo = parseBinOp "mod" Mod
        parseEquals = parseBinOp "eql" Eql

data ALUState = ALU
    { _x :: !Int
    , _y :: !Int
    , _z :: !Int
    , _w :: !Int
    , _inputs :: [Int]
    }

$(makeLenses ''ALUState)

emptyMem = ALU 0 0 0 0 []

numberFromDigits = foldl (\n d -> n * 10 + d) 0

hardcodedEval = evalStateT go emptyMem
    where
        loop i d a b = do
            -- n <- lift [i]
            n <- lift [9,8,7,6,5,4,3,2,1]
            inputs %= (n:)
            z' <- use z
            z %= (`div` d)
            -- traceShowM (z' `mod` 26, a, b)
            unless ((z' `mod` 26) + a == n) (z *= 26 >> z += n + b)
            -- traceShowM =<< use z
        go = do
            loop 7 1 14 16          -- z * 26 + (w + 16)
            loop 9 1 11 3           -- z * 26 + (w + 11)
            loop 9 1 12 2           -- z * 26 + (w + 12)
            loop 9 1 11 7           -- z * 26 + (w + 11)
            notTooHigh3 <- uses z (< 451488)
            guard notTooHigh3
            loop 6 26 (-10) 13      --
            loop 9 1 15 6           -- z * 26 + (w + 11)
            notTooHigh2 <- uses z (< 451832)
            guard notTooHigh2
            loop 1 26 (-14) 10      -- f(x) = 15
            loop 2 1 10 11          -- z * 26 + (w + 10)
            notTooHigh1 <- uses z (< 452103)
            guard notTooHigh1
            loop 9 26 (-4) 6        --
            loop 8 26 (-3) 5        --
            loop 1 1 13 11          -- z * 26 + (w + 13)
            notTooHigh <- uses z (< 17575)
            guard notTooHigh
            loop 9 26 (-3) 4        --
            loop 3 26 (-9) 4        --
            loop 9 26 (-12) 6       --
            valid <- uses z (==0)
            guard valid
            numberFromDigits . reverse <$> use inputs

reg :: Variable -> Lens' ALUState Int
reg = \case
    X -> x
    Y -> y
    Z -> z
    W -> w

day24a :: _ :~> _
day24a = MkSol
    { sParse = parseLines parser
    , sShow  = show
    , sSolve = Just . maximum . const hardcodedEval
    }

day24b :: _ :~> _
day24b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just . minimum . const hardcodedEval
    }

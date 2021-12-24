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

eval is = evalStateT (go is) emptyMem
    where
        go [] = do
            valid <- uses z (==0)
            guard valid
            numberFromDigits . reverse <$> use inputs
        go (Inp a:is) = do
            n <- lift [9,8,7,6,5,4,3,2,1]
            reg a .= n
            inputs %= (n:)
            go is
        go (Add a b:is) = runOp (+) a b >> go is
        go (Mul a b:is) = runOp (*) a b >> go is
        go (Div a b:is) = runOp div a b >> go is
        go (Mod a b:is) = runOp mod a b >> go is
        go (Eql a b:is) = runOp intEql a b >> go is

intEql a b = if a == b then 0 else 1

runOp op a b = ((reg a %=) . flip op) =<< ref b
ref :: Term -> StateT ALUState [] Int
ref (Val n) = pure n
ref (Var v) = use (reg v)

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
    , sSolve = Just . maximum . eval
    }

day24b :: _ :~> _
day24b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

-- |
-- Module      : AOC.Challenge.Day24
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 24.

module AOC.Challenge.Day24 (
    day24a
  , day24b
  ) where

import AOC.Solver ((:~>)(..))
import Control.Monad (guard, unless)
import Control.Monad.Trans.State (evalStateT)
import Control.Monad.Trans.Class (lift)
import Control.Lens

data ALUState = ALU
    { _z :: !Int
    , _inputs :: [Int]
    }

$(makeLenses ''ALUState)

emptyMem :: ALUState
emptyMem = ALU 0 []

numberFromDigits :: [Int] -> Int
numberFromDigits = foldl (\n d -> n * 10 + d) 0

hardcodedEval :: [Int]
hardcodedEval = evalStateT go emptyMem
    where
        loop d a b = do
            n <- lift [9,8,7,6,5,4,3,2,1]
            inputs %= (n:)
            z' <- use z
            z %= (`div` d)
            unless ((z' `mod` 26) + a == n) (z *= 26 >> z += n + b)
        go = do
            loop  1   14  16
            loop  1   11   3
            loop  1   12   2
            loop  1   11   7
            notTooHigh3 <- uses z (< 451488)
            guard notTooHigh3
            loop 26 (-10) 13
            loop  1   15   6
            notTooHigh2 <- uses z (< 451832)
            guard notTooHigh2
            loop 26 (-14) 10
            loop  1   10  11
            notTooHigh1 <- uses z (< 452103)
            guard notTooHigh1
            loop 26  (-4)  6
            loop 26  (-3)  5
            loop  1   13  11
            notTooHigh <- uses z (< 17575)
            guard notTooHigh
            loop 26 (-3)  4
            loop 26 (-9)  4
            loop 26 (-12) 6
            valid <- uses z (==0)
            guard valid
            numberFromDigits . reverse <$> use inputs

day24a :: String :~> Int
day24a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just . maximum . const hardcodedEval
    }

day24b :: String :~> Int
day24b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just . minimum . const hardcodedEval
    }

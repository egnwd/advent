{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.

module AOC.Challenge.Day12 (
    day12a
  , day12b
  ) where

import           AOC.Common           (CharParser, pDecimal, pTok, parseLines)
import           AOC.Solver           ((:~>) (..))
import           Control.Lens         (at, ix, makeLenses, to, use, uses, (+=),
                                       (-=), (.=), (^.))
import           Control.Monad        (when)
import           Control.Monad.State  (State, evalState)
import           Data.IntMap          (IntMap)
import qualified Data.IntMap          as IM
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Text.Megaparsec      (choice, (<|>))
import           Text.Megaparsec.Char

data Instruction
    = Cpy Ref Reg
    | Inc Reg
    | Dec Reg
    | Jnz Ref Int
    deriving (Show)

data Reg = A | B | C | D deriving (Show, Eq, Ord, Enum)
data Ref = Reg Reg | Val Int deriving (Show)
type Program = IntMap Instruction

parser :: CharParser Instruction
parser = choice [parseCopy, parseInc, parseDec, parseJnz]

parseReg :: CharParser Reg
parseReg = pTok $ choice [A <$ char 'a', B <$ char 'b', C <$ char 'c', D <$ char 'd']

parseRef :: CharParser Ref
parseRef = pTok $ (Reg <$> parseReg) <|> (Val <$> pDecimal)

parseCopy, parseInc, parseDec, parseJnz :: CharParser Instruction
parseCopy = "cpy " *> (Cpy <$> parseRef <*> parseReg)
parseInc = "inc " *> (Inc <$> parseReg)
parseDec = "dec " *> (Dec <$> parseReg)
parseJnz = "jnz " *> (Jnz <$> parseRef <*> pDecimal)

data EvalutationState
    = CPU
        { _program   :: Program
        , _pointer   :: Int
        , _registers :: Map Reg Int
        }

$(makeLenses ''EvalutationState)

evalProgram :: EvalutationState -> Map Reg Int
evalProgram state = evalState go state
    where
        n = state ^. program . to IM.size
        go :: State EvalutationState _
        go = do
            p <- use pointer
            if p >= n
               then use registers
               else do
                   uses program (IM.! p) >>= evalInstruction
                   pointer += 1
                   go

        evalInstruction :: Instruction -> State EvalutationState _
        evalInstruction (Cpy ref reg) = evalReference ref >>= \v -> registers . at reg .= v
        evalInstruction (Inc reg) = registers . ix reg += 1
        evalInstruction (Dec reg) = registers . ix reg -= 1
        evalInstruction (Jnz ref off) = do
            v <- evalReference ref
            when (v /= Just 0) (pointer += off-1)

        evalReference (Val v) = pure (Just v)
        evalReference (Reg r) = use (registers . at r)

day12a :: Program :~> Int
day12a = MkSol
    { sParse = fmap (IM.fromList . zip [0..]) . parseLines parser
    , sShow  = show
    , sSolve = M.lookup A . evalProgram . mkState (0,0,0,0)
    }

day12b :: Program :~> Int
day12b = MkSol
    { sParse = fmap (IM.fromList . zip [0..]) . parseLines parser
    , sShow  = show
    , sSolve = M.lookup A . evalProgram . mkState (0,0,1,0)
    }

mkState :: (Int, Int, Int, Int) -> Program -> EvalutationState
mkState (a,b,c,d) prg = CPU prg 0 (M.fromList [(A,a),(B,b),(C,c),(D,d)])

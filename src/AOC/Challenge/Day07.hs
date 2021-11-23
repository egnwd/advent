-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.

module AOC.Challenge.Day07 (
    day07a
  , day07b
  ) where

import AOC.Solver ((:~>)(..), dyno_)
import AOC.Common
    ( CharParser, pTok, pWord, pDecimal, parseLinesOrError )
import Data.Bits ( Bits(complement, (.&.), (.|.), shiftL, shiftR) )
import Data.Word ( Word16 )
import Data.Map (Map, (!))
import qualified Data.Map as M
import Control.Monad.Combinators ( (<|>), choice )
import Text.Megaparsec (try)
import Text.Megaparsec.Char ( string )

type Wire = String
type Signal = Word16
data Input = Constant Signal | Name Wire deriving (Eq, Show)
data Command
    = Set Input
    | And Input Input
    | Or  Input Input
    | LShift Input Input
    | RShift Input Input
    | Not Input
    deriving (Show, Eq)

type Instruction = (Wire, Command)

arrow :: CharParser String
arrow = pTok $ string "->"

pInput :: CharParser Input
pInput = (Constant <$> pTok pDecimal) <|> (Name <$> pTok pWord)

pOp :: CharParser (Input -> Input -> Command)
pOp = choice $ map (\(a, b) -> a <$ pTok (string b))
    [ (And, "AND")
    , (Or, "OR")
    , (LShift, "LSHIFT")
    , (RShift, "RSHIFT")
    ]

pSet :: CharParser Instruction
pSet = try $ do
    i <- Set <$> (pInput <* arrow)
    dst <- pWord
    pure (dst, i)

pBinOp :: CharParser Instruction
pBinOp = try $ do
    a <- pInput
    op <- pOp
    b <- pInput
    arrow
    dst <- pWord
    pure (dst, op a b)

pNot :: CharParser Instruction
pNot = try $ do
    pTok $ string "NOT"
    a <- pInput
    arrow
    dst <- pWord
    pure (dst, Not a)

parse :: CharParser Instruction
parse = choice [pSet, pBinOp, pNot]

parta :: Wire -> Map Wire Command -> Maybe Signal
parta w is = M.lookup w circuit
    where
        circuit = M.map eval is
        eval (Set src)           = evalInput src
        eval (And srcA srcB)     = bin (.&.) srcA srcB
        eval (Or srcA srcB)      = bin (.|.) srcA srcB
        eval (LShift srcA srcB)  = shiftL (evalInput srcA) (fromIntegral . evalInput $ srcB)
        eval (RShift srcA srcB)  = shiftR (evalInput srcA) (fromIntegral . evalInput $ srcB)
        eval (Not src)           = complement . evalInput $ src
        evalInput (Constant w')   = w'
        evalInput (Name w')       = circuit ! w'
        bin op a b = evalInput a `op` evalInput b

partb :: Wire -> Map Wire Command -> Maybe Signal
partb a is = parta a is >>= (\a' -> parta a (M.insert "b" a' is)) . Set . Constant

day07a :: Map Wire Command :~> Signal
day07a = MkSol
    { sParse = fmap M.fromList . parseLinesOrError parse
    , sShow  = show
    , sSolve = parta (dyno_ "wire" "a")
    }

day07b :: Map Wire Command :~> Signal
day07b = MkSol
    { sParse = fmap M.fromList . parseLinesOrError parse
    , sShow  = show
    , sSolve = partb (dyno_ "wire" "a")
    }

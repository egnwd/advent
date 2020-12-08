{-# LANGUAGE TemplateHaskell #-}
{-|
   Name: Handheld Halting
   Url: <https://adventofcode.com/2020/day/8>
-}

module Day08 (main) where

import Control.Monad.State

import Advent
import Prelude hiding (unlines)

import Control.Bool
import Control.Lens
import Control.Applicative
import Data.Maybe
import Data.List.Zipper
import Data.Function.HT
import qualified Data.Set as S

data InstrState = S { _sAcc :: Int, _sSeen :: S.Set Int, _sFinished :: Bool }
$(makeLenses ''InstrState)

data Instr = Jmp !Int | Acc !Int | Nop !Int deriving (Show, Eq)
type Tape = Zipper (Int, Instr)
type Eval a = State InstrState a
type Input  = [Instr]
type Output = Int

main :: IO ()
main = do
  input <- getParsedLines 8 parseInput
  print $ part1 input
  print $ part2 input

initialState = S 0 mempty False

-- | Parsing
parseInput :: Parser Instr
parseInput = Jmp <$> (parseNumber "jmp") <|> Acc <$> (parseNumber "acc") <|> Nop <$> (parseNumber "nop")
  where parseNumber name = fromIntegral <$> (symbol name *> number)

part1 :: Input -> Output
part1 input = execState (eval . mkTape $ input) initialState ^. sAcc

part2 :: Input -> Output
part2 input = fromJust . findTape . makeOptions $ input

findTape [] = Nothing
findTape (is:iss) = let s = execState (eval . mkTape $ is) initialState
                     in if s ^. sFinished then Just (s ^. sAcc) else findTape iss

mkTape = fromList . zip [0..]

eval :: Tape -> Eval Tape
eval z =
  case safeCursor z of
    Nothing -> sFinished .= True >> return z
    Just (i, instr) ->
      ifThenElseM
        ((S.member i) <$> use sSeen)
        (return z)
        (sSeen %= S.insert i >> exec z instr >>= eval)

exec z (Jmp n) = return $ jump n z
exec z (Acc n) = sAcc += n >> return (right z)
exec z (Nop _) = return (right z)

jump :: Int -> Tape -> Tape
jump n z
  | n < 0     = nest (-n) left z
  | otherwise = nest n right z

makeOptions [] = []
makeOptions (x:xs) = (fixInstr x : xs) : (map (x :) (makeOptions xs))

fixInstr (Jmp n) = (Nop n)
fixInstr (Acc n) = (Acc n)
fixInstr (Nop n) = (Jmp n)

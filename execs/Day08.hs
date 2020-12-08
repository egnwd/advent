{-|
   Name: Handheld Halting
   Url: <https://adventofcode.com/2020/day/8>
-}

module Day08 (main) where

import Control.Monad.State

import Advent
import Prelude hiding (unlines)

import Data.Maybe
import Data.List.Zipper
import Data.Function.HT
import qualified Data.Set as S

import Text.Megaparsec (choice)

main :: IO ()
main = do
  input <- getParsedLines 8 parseInput
  print $ part1 input
  print $ part2 input

data Instr = Jmp !Int | Acc !Int | Nop !Int deriving (Show, Eq)
type Tape = Zipper (Int, Instr)
type Eval a = State InstrState a
type Input  = [Instr]
type Output = Int

data InstrState = S { sAcc :: Int, sSeen :: S.Set Int, sFinished :: Bool }

initialState = S 0 mempty False

-- | Parsing
parseInput :: Parser Instr
parseInput = choice
         [ Jmp . fromIntegral <$> (symbol "jmp" *> number)
         , Acc . fromIntegral <$> (symbol "acc" *> number)
         , Nop . fromIntegral <$> (symbol "nop" *> number)
         ]

part1 :: Input -> Output
part1 input = sAcc $ execState (eval . mkTape $ input) initialState

part2 :: Input -> Output
part2 input = fromJust . findTape . makeOptions $ input

findTape [] = Nothing
findTape (is:iss) = let s = execState (eval . mkTape $ is) initialState
                     in if sFinished s then Just (sAcc s) else findTape iss

mkTape = fromList . zip [0..]

eval :: Tape -> Eval Tape
eval z = do
  let c = safeCursor z
  case c of
    Nothing -> modify (\s -> s { sFinished = True }) >> return z
    Just (i, instr) -> do
      seen <- gets $ S.member i . sSeen
      if seen
         then return z
         else do
           modify (\s -> s { sSeen = S.insert i (sSeen s) })
           case instr of
             Jmp n -> return (jump n z) >>= eval
             Acc n -> modify (\s -> s { sAcc = (sAcc s) + n }) >> return (right z) >>= eval
             Nop _ -> return (right z) >>= eval

jump :: Int -> Tape -> Tape
jump n z
  | n < 0     = nest (-n) left z
  | otherwise = nest n right z

makeOptions [] = []
makeOptions (x:xs) = (fixInstr x : xs) : (map (x :) (makeOptions xs))

fixInstr (Jmp n) = (Nop n)
fixInstr (Acc n) = (Acc n)
fixInstr (Nop n) = (Jmp n)

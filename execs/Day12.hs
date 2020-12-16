{-# LANGUAGE RankNTypes, TemplateHaskell #-}
{-|
   Name: Rain Risk
   Url: <https://adventofcode.com/2020/day/12>
-}

module Day12 (main) where

import Advent
import Advent.Compass
import Prelude hiding (unlines, Either(..))
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad.State
import Linear ((*^), V2(..))
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char)
import Data.Group

data Instruction = Move Point | Turn Cardinality | Forward Int deriving Show
type Input  = [Instruction]
type Output = Int

data CourseState = Course { _ship, _waypoint :: Point, _bearing :: Cardinality }
$(makeLenses ''CourseState)

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedLines 12 parseInput

-- | Parsing
parseInput :: Parser Instruction
parseInput = (Forward <$> (char 'F' *> number)) <|> (Turn <$> (parseDirection <*> number)) <|> (Move <$> (parseMovement <*> number))
  where
    parseMovement = flip (*^) . toVec <$> parseCardinality
    parseDirection = (pow West . (`div` 90) <$ char 'L') <|> (pow East . (`div` 90) <$ char 'R')

part1 :: Input -> Output
part1 = setSail ship (V2 0 1)

part2 :: Input -> Output
part2 = setSail waypoint (V2 1 10)

setSail :: Lens' CourseState Point -> Point -> Input -> Output
setSail drifter w input = let course = execState (traverse (runInstruction drifter) input) (Course origin w East)
                           in mhtnDist origin $ course ^. ship

runInstruction drifter (Move v) = drifter += v
runInstruction _ (Forward i)    = use waypoint >>= moveForward i
runInstruction _ (Turn c)       = bearing %= (c <>) >> waypoint %= rotate c

moveForward :: Int -> V2 Int -> State CourseState ()
moveForward i w = ship += (i *^ w)

mhtnDist :: V2 Int -> V2 Int -> Int
mhtnDist o = sum . abs . (o-)

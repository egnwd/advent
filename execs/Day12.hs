{-# LANGUAGE RankNTypes, TemplateHaskell #-}
{-|
   Name: Rain Risk
   Url: <https://adventofcode.com/2020/day/12>
-}

module Day12 (main) where

import Advent
import Prelude hiding (unlines, Either(..))
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad.State
import Linear.V2
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char)

data Cardinality = North | East | South | West deriving (Show, Enum, Bounded)
data Position = Pos Cardinality Int deriving Show
data Direction = Left | Right | Forward deriving Show
data Movement = Move Direction Int
data Instruction = Drift Position | Shift Movement
type Input  = [Instruction]
type Output = Int

data CourseState = Course { _ship, _waypoint :: V2 Int, _bearing :: Cardinality }
$(makeLenses ''CourseState)

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedLines 12 parseInput

-- | Parsing
parseInput :: Parser Instruction
parseInput = (Drift <$> parsePosition) <|> (Shift <$> parseMovement)
  where
    parsePosition = Pos <$> parseCardinality <*> (fromIntegral <$> number)
    parseMovement = Move <$> parseDirection <*> (fromIntegral <$> number)
    parseCardinality = choice [ North <$ char 'N', East <$ char 'E', South <$ char 'S', West <$ char 'W' ]
    parseDirection = choice [ Left <$ char 'L', Right <$ char 'R', Forward <$ char 'F' ]

origin = V2 0 0

part1 :: Input -> Output
part1 = setSail ship (V2 0 1)

part2 :: Input -> Output
part2 = setSail waypoint (V2 1 10)

setSail drifter w input = let course = execState (runShip (applyDrift drifter) input) (Course origin w East)
                           in mhtnDist origin $ course ^. ship

runShip _ [] = return []
runShip drifter (Drift pos:is) = modify (drifter pos) >> runShip drifter is
runShip drifter (Shift mov:is) = applyShift mov >> runShip drifter is

applyDrift l (Pos North i) = l . _x +~ i
applyDrift l (Pos South i) = l . _x -~ i
applyDrift l (Pos East i)  = l . _y +~ i
applyDrift l (Pos West i)  = l . _y -~ i

applyShift :: Movement -> State CourseState ()
applyShift (Move Left i) = performN (i `div` 90) (bearing %= goLeft >> waypoint %= rotateLeft)
applyShift (Move Right i) = performN (i `div` 90) (bearing %= goRight >> waypoint %= rotateRight)
applyShift (Move Forward i) = use waypoint >>= modify . moveForward (pure i)

goLeft North = West
goLeft c = pred c

goRight West = North
goRight c = succ c

rotateLeft (V2 x y) = V2 y (-x)
rotateRight (V2 x y) = V2 (-y) (x)

moveForward :: V2 Int -> V2 Int -> CourseState -> CourseState
moveForward i w = ship +~ (w * i)

mhtnDist :: V2 Int -> V2 Int -> Int
mhtnDist o = sum . abs . (o-)

performN 0 _ = return ()
performN n m = m >> performN (n-1) m

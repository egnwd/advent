{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import AOC.Solver          ((:~>)(..))
import AOC.Common          (parseLines, pTok, pDecimal, Point, CharParser)
import Data.Foldable       (for_)
import Linear              (V2(..))
import Text.Megaparsec     (try, choice)
import Control.Monad.State (execState)
import Data.Monoid         (Sum(..))
import Control.Lens        (makeLenses, (+=), use)

data Direction = Forward | Up | Down | Backward deriving Eq

type Instruction = (Direction, Int)

data Submarine = Sub { _loc :: Point, _aim :: Int }

$(makeLenses ''Submarine)

initialSubmarine :: Submarine
initialSubmarine = Sub (pure 0) 0

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

solveb :: [Instruction] -> Point
solveb is = _loc $ flip execState initialSubmarine $
    for_ is (\(d, n) -> do
        a <- use aim
        loc += pure n * positionUpdate a d
        aim += n * aimUpdate d)

    where
        positionUpdate a = \case
            Forward  -> V2 1    a
            Backward -> V2 (-1) 0
            _        -> V2 0    0

        aimUpdate = \case
            Up   -> -1
            Down -> 1
            _    -> 0

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
    , sSolve = Just . product . solveb
    }

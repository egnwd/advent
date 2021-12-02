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
import Data.Foldable       (traverse_)
import Linear              (V2(..))
import Control.Applicative ((<|>))
import Control.Monad.State (execState)
import Control.Lens        (makeLenses, (+=), use)

data Direction = Forward | Up | Down deriving Eq

type Instruction = (Direction, Int)

data Submarine = Sub { _loc :: Point, _aim :: Int }

$(makeLenses ''Submarine)

initialSubmarine :: Submarine
initialSubmarine = Sub (pure 0) 0

parser :: CharParser Instruction
parser = (,) <$> pTok ((Forward <$ "forward") <|> (Up <$ "up") <|> (Down <$ "down")) <*> pDecimal

solve :: [Instruction] -> Point
solve = sum . map (\(d,n) -> pure n * positionUpdate d)
    where
        positionUpdate = \case
            Forward -> V2 1 0
            Up      -> V2 0 (-1)
            Down    -> V2 0 1

solveb :: [Instruction] -> Point
solveb = _loc . flip execState initialSubmarine . traverse_ (uncurry moveSubmarine)
    where
        moveSubmarine Forward n = use aim >>= \a -> loc += V2 n (n*a)
        moveSubmarine d       n = aim += n * aimUpdate d

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

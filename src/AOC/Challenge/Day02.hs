{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import           AOC.Common                (CharParser, pSpace, parseLines)
import           AOC.Solver                ((:~>)(..))
import           Control.Monad.Combinators ((<|>))
import           Data.Monoid               (Sum(..))

data Outcome = Win | Draw | Lose deriving (Eq, Ord, Show)

data RPS = Rock | Paper | Scissors deriving (Eq, Show)

plays :: RPS -> RPS -> Outcome
Paper    `plays` Rock     = Win
Rock     `plays` Scissors = Win
Scissors `plays` Paper    = Win
a        `plays` b
  | a == b = Draw
  | otherwise = Lose

whatToPlay :: RPS -> Outcome -> RPS
whatToPlay a        Draw = a
whatToPlay Paper    Win  = Scissors
whatToPlay Rock     Win  = Paper
whatToPlay Scissors Win  = Rock
whatToPlay Paper    Lose = Rock
whatToPlay Rock     Lose = Scissors
whatToPlay Scissors Lose = Paper

score :: RPS -> Outcome -> Sum Int
score you out = Sum $ scoreRPS you + scoreOutcome out
    where
        scoreRPS :: RPS -> Int
        scoreRPS = \case
            Rock     -> 1
            Paper    -> 2
            Scissors -> 3

        scoreOutcome :: Outcome -> Int
        scoreOutcome = \case
            Win  -> 6
            Draw -> 3
            Lose -> 0

day02a :: [(RPS, RPS)] :~> Int
day02a = MkSol
    { sParse = parseLines parseGame
    , sShow  = show
    , sSolve = Just . getSum . foldMap (\(opp, you) -> score you (you `plays` opp))
    }

day02b :: [(RPS, Outcome)] :~> Int
day02b = MkSol
    { sParse = parseLines parseStrategy
    , sShow  = show
    , sSolve = Just . getSum . foldMap (\(opp, out) -> score (whatToPlay opp out) out)
    }

parseGame :: CharParser (RPS, RPS)
parseGame = do
    opp <- Rock <$ "A" <|> Paper <$ "B" <|> Scissors <$ "C"
    pSpace
    you <- Rock <$ "X" <|> Paper <$ "Y" <|> Scissors <$ "Z"
    return (opp, you)

parseStrategy :: CharParser (RPS, Outcome)
parseStrategy = do
    opp <- Rock <$ "A" <|> Paper <$ "B" <|> Scissors <$ "C"
    pSpace
    out <- Lose <$ "X" <|> Draw <$ "Y" <|> Win <$ "Z"
    return (opp, out)

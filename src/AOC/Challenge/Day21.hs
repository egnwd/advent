{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

-- |
-- Module      : AOC.Challenge.Day21
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 21.

module AOC.Challenge.Day21 (
    day21a
  , day21b
  ) where

import AOC.Solver ((:~>)(..))
import AOC.Common (parseMaybeLenient, CharParser, (!?), freqs, loopEither, pDecimal)
import Control.DeepSeq
import GHC.Generics
import Control.Lens
import Control.Monad.State
import Data.Map (Map)
import Data.Maybe
import Linear
import Data.Bifunctor
import Data.Semigroup
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Map as M

data GameState = DiracDice
    { _player1Loc :: Int
    , _player2Loc :: Int
    , _player1Score :: Int
    , _player2Score :: Int
    , _numberOfRolls :: Int
    , _winner :: Maybe Int
    } deriving (Show, Generic)

data QuantumGameState = QuantumGameState { _player1 :: (Int, Int)
                                         , _player2 :: (Int, Int)
                                         } deriving (Eq, Ord, Generic)

instance NFData GameState
instance NFData QuantumGameState

$(makeLenses ''GameState)
$(makeLenses ''QuantumGameState)

initialState l1 l2 = DiracDice l1 l2 0 0 0 Nothing

parser :: CharParser GameState
parser = do
    l1 <- "Player 1 starting position: " *> pDecimal <* newline
    l2 <- "Player 2 starting position: " *> pDecimal
    pure $ initialState l1 l2

quantumParser :: CharParser QuantumGameState
quantumParser = do
    l1 <- "Player 1 starting position: " *> pDecimal <* newline
    l2 <- "Player 2 starting position: " *> pDecimal
    pure $ QuantumGameState (l1, 0) (l2, 0)

diceRoll nth = x + y + z
    where
        x = loop 100 nth 1
        y = loop 100 nth 2
        z = loop 100 nth 3

loop n i m = ((i + m - 1) `mod` n) + 1
{-# INLINE loop #-}

quantumDiceRolls :: Map Int Int
quantumDiceRolls = freqs $ sum <$> (V3 <$> [1..3] <*> [1..3] <*> [1..3])

simQuantum :: Int -> (Int, Int) -> State QuantumGameState (Either Int (V2 Int))
simQuantum p (roll, n) = do
    let getP = if p == 1 then player1 else player2
    let winP = if p == 1 then V2 n 0 else V2 0 n
    loc <- getP . _1 <%= loop 10 roll
    score <- getP . _2 <+= loc
    if score >= 21
       then pure (Right winP)
       else pure (Left n)

step :: QuantumGameState -> V2 Int
step s = loopEither go (V2 0 0, M.singleton s 1)
    where
        go :: (V2 Int, Map QuantumGameState Int) -> Either (V2 Int) (V2 Int, Map QuantumGameState Int)
        go (w,m) = do
            let (wins, states) = bimap sum (M.unionsWith (+)) . unzip . map stepTurn . M.toList $ m
            let totalWins = w + wins
            if null states
               then Left totalWins
               else Right (totalWins, states)

stepTurn :: (QuantumGameState, Int) -> (V2 Int, Map QuantumGameState Int)
stepTurn s0@(_,n) = (wins + wins', states')
    where
        (wins, states) = stepPlayer 1 s0
        (wins', states') = bimap sum (M.unionsWith (+)) . unzip . map (stepPlayer 2) . M.toList $ states

stepPlayer :: Int -> (QuantumGameState, Int) -> (V2 Int, Map QuantumGameState Int)
stepPlayer p (s, n) = (n *^ totalWins, (*n) <$> M.fromListWith (+) unfinishedGames)
    where
        totalWins = maybe 0 getSum (foldMap (fmap Sum) wins)
        (wins, unfinishedGames) = second catMaybes . unzip . map simulateGame . M.toList $ quantumDiceRolls
        simulateGame x = ditchFinishedGames $ runState (simQuantum p x) s
        ditchFinishedGames (Left n, s) = (Nothing, Just (s,n))
        ditchFinishedGames (Right w, _) = (Just w, Nothing)

scoreGame :: GameState -> Maybe Int
scoreGame DiracDice{ _winner = (Just 1), _player2Score, _numberOfRolls } = Just (_numberOfRolls * _player2Score)
scoreGame DiracDice{ _winner = (Just 2), _player1Score, _numberOfRolls } = Just (_numberOfRolls * _player1Score)
scoreGame _ = Nothing

simDiracDice :: GameState -> GameState
simDiracDice s = execState ?? s $ sim
    where
        sim = do
            roll1 <- diceRoll <$> (numberOfRolls <<+= 3)
            l1 <- player1Loc <%= loop 10 roll1
            s1 <- player1Score <+= l1
            if s1 >= 1000
               then winner .= Just 1
               else do
                   roll2 <- diceRoll <$> (numberOfRolls <<+= 3)
                   l2 <- player2Loc <%= loop 10 roll2
                   s2 <- player2Score <+= l2
                   if s2 >= 1000 then winner .= Just 2 else sim

day21a :: _ :~> _
day21a = MkSol
    { sParse = parseMaybeLenient parser
    , sShow  = show
    , sSolve = scoreGame . simDiracDice
    }

day21b :: _ :~> _
day21b = MkSol
    { sParse = parseMaybeLenient quantumParser
    , sShow  = show
    , sSolve = Just . maximum . step
    }

example = "Player 1 starting position: 4\nPlayer 2 starting position: 8"
input = "Player 1 starting position: 10\nPlayer 2 starting position: 7"

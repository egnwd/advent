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
import AOC.Common (parseMaybeLenient, CharParser, freqs, loopEither, pDecimal)
import Control.Lens
import Control.Monad.State
import Data.Map (Map)
import Data.Maybe
import Linear
import Data.Bifunctor
import Data.Semigroup
import Text.Megaparsec.Char
import qualified Data.Map as M

data Players = Player1 | Player2 deriving (Eq, Ord)
data DiracDiceState = DiracDice { _player1 :: (Int, Int)
                                  , _player2 :: (Int, Int)
                                  , _numberOfRolls :: Int
                                  , _winner :: Maybe Players
                                  } deriving (Eq, Ord)

$(makeLenses ''DiracDiceState)

parser :: CharParser DiracDiceState
parser = do
    l1 <- "Player 1 starting position: " *> pDecimal <* newline
    l2 <- "Player 2 starting position: " *> pDecimal
    pure $ DiracDice (l1, 0) (l2, 0) 0 Nothing

diceRoll :: Int -> Int
diceRoll nth = x + y + z
    where
        x = loop 100 nth 1
        y = loop 100 nth 2
        z = loop 100 nth 3

loop :: Int -> Int -> Int -> Int
loop n i m = ((i + m - 1) `mod` n) + 1
{-# INLINE loop #-}

quantumDiceRolls :: Map Int Int
quantumDiceRolls = freqs $ sum <$> (V3 <$> [1..3] <*> [1..3] <*> [1..3])

simQuantum :: Players -> (Int, Int) -> State DiracDiceState (Either Int (V2 Int))
simQuantum p (roll, n) = do
    let getP = if p == Player1 then player1 else player2
    let winP = if p == Player1 then V2 n 0 else V2 0 n
    loc <- getP . _1 <%= loop 10 roll
    score <- getP . _2 <+= loc
    if score >= 21
       then pure (Right winP)
       else pure (Left n)

simQuantumDice :: DiracDiceState -> V2 Int
simQuantumDice s = loopEither go (V2 0 0, M.singleton s 1)
    where
        go :: (V2 Int, Map DiracDiceState Int) -> Either (V2 Int) (V2 Int, Map DiracDiceState Int)
        go (w,m) = do
            let (wins, states) = bimap sum (M.unionsWith (+)) . unzip . map stepTurn . M.toList $ m
            let totalWins = w + wins
            if null states
               then Left totalWins
               else Right (totalWins, states)

stepTurn :: (DiracDiceState, Int) -> (V2 Int, Map DiracDiceState Int)
stepTurn s0 = (wins + wins', states')
    where
        (wins, states) = stepPlayer Player1 s0
        (wins', states') = bimap sum (M.unionsWith (+)) . unzip . map (stepPlayer Player2) . M.toList $ states

stepPlayer :: Players -> (DiracDiceState, Int) -> (V2 Int, Map DiracDiceState Int)
stepPlayer p (s, n) = (n *^ totalWins, (*n) <$> M.fromListWith (+) unfinishedGames)
    where
        totalWins = maybe 0 getSum (foldMap (fmap Sum) wins)
        (wins, unfinishedGames) = second catMaybes . unzip . map simulateGame . M.toList $ quantumDiceRolls
        simulateGame x = ditchFinishedGames $ runState (simQuantum p x) s
        ditchFinishedGames (Left n, s) = (Nothing, Just (s,n))
        ditchFinishedGames (Right w, _) = (Just w, Nothing)

scoreGame :: DiracDiceState -> Maybe Int
scoreGame dd = score <&> (* (dd ^. numberOfRolls))
    where
         score = dd ^. winner <&> \case
             Player1 -> dd ^. player2 . _2
             Player2 -> dd ^. player1 . _2

simDiracDice :: DiracDiceState -> DiracDiceState
simDiracDice = execState sim
    where
        sim = do
            roll <- diceRoll <$> (numberOfRolls <<+= 3)
            loc <- player1 . _1 <%= loop 10 roll
            score <- player1 . _2 <+= loc
            if score >= 1000
               then winner .= Just Player1
               else flipPlayers >> sim >> flipWinner
        flipPlayers = do
            p1 <- use player1
            p2 <- use player2
            player1 .= p2
            player2 .= p1
        flipWinner =
            winner %= fmap \case
                 Player1 -> Player2
                 Player2 -> Player1

day21a :: DiracDiceState :~> Int
day21a = MkSol
    { sParse = parseMaybeLenient parser
    , sShow  = show
    , sSolve = scoreGame . simDiracDice
    }

day21b :: DiracDiceState :~> Int
day21b = MkSol
    { sParse = parseMaybeLenient parser
    , sShow  = show
    , sSolve = Just . maximum . simQuantumDice
    }

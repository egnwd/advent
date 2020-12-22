{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-|
   Name: Crab Combat
   Url: <https://adventofcode.com/2020/day/22>
-}

module Day22 (main) where

import Advent
import Control.Arrow ((&&&))
import Control.Monad.State
import Control.Lens hiding ((:<))
import Data.Sequence (ViewL(..))
import qualified Data.Sequence as S
import qualified Data.Set as SS
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char
import Data.Monoid
import Data.Maybe

type Input  = (Deck, Deck)
type Output = Int

type Deck = S.Seq Int
data GameOver = No | WinPlayer1 | WinPlayer2
data GameState = Combat
  { _player1 :: Deck
  , _player2 :: Deck
  , _history :: SS.Set (Deck, Deck)
  , _gameOver :: GameOver
  }
$(makeLenses ''GameState)

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedInput 22 parseInput

part1 :: Input -> Output
part1 = ans . uncurry combat
part2 :: Input -> Output
part2 = ans . fst . uncurry recursiveCombat

loop :: StateT GameState Maybe Deck
loop = do
  p1 <- drawCard player1
  p2 <- drawCard player2
  playRoundHigher p1 p2 >>= putToBottom p1 p2
  checkGameOver >>= \case
    No         -> loop
    WinPlayer1 -> use player1
    WinPlayer2 -> use player2

recursiveLoop :: StateT GameState Maybe Deck
recursiveLoop = do
  p1 <- use player1
  p2 <- use player2
  checkWinByHistory p1 p2 >>= \case
    WinPlayer1 -> return p1
    No -> do
      t1 <- drawCard player1
      t2 <- drawCard player2
      playRound t1 t2 >>= putToBottom t1 t2
      checkGameOver >>= \case
        No         -> recursiveLoop
        WinPlayer1 -> use player1
        WinPlayer2 -> use player2

playRound t1 t2 = do
  l1 <- uses player1 S.length
  l2 <- uses player2 S.length
  if t1 <= l1 && t2 <= l2
     then view (_2 . gameOver) <$> (recursiveCombat <$> uses player1 (S.take t1) <*> uses player2 (S.take t2))
     else playRoundHigher t1 t2

playRoundHigher t1 t2
  | t1 > t2   = return WinPlayer1
  | otherwise = return WinPlayer2

checkGameOver = do
  p1 <- use player1
  p2 <- use player2
  if | S.null p1 -> gameOver <#= WinPlayer2
     | S.null p2 -> gameOver <#= WinPlayer1
     | otherwise -> gameOver <#= No

putToBottom p1 p2 WinPlayer1 = player1 %= \d -> d |> p1 |> p2
putToBottom p1 p2 WinPlayer2 = player2 %= \d -> d |> p2 |> p1

drawCard :: Lens' GameState Deck -> StateT GameState Maybe Int
drawCard player = do
  p :< ps <- uses player S.viewl
  player #= ps
  return p

checkWinByHistory p1 p2 = do
  seen <- uses history (SS.member (p1,p2))
  history %= SS.insert (p1,p2)
  if seen then gameOver <#= WinPlayer1 else gameOver <#= No

initialState p1 p2 = Combat p1 p2 mempty No

combat p1 p2 = fromJust . evalStateT loop $ initialState p1 p2
recursiveCombat p1 p2 = fromJust . runStateT recursiveLoop $ initialState p1 p2

ans = getSum . S.foldMapWithIndex (\i a -> Sum ((i+1) * a)) . S.reverse

-- | Parsing
parseInput :: Parser Input
parseInput = (,) <$> parsePlayer <* newline <* newline <*> parsePlayer
  where
    parsePlayer = do
      symbol "Player" <* number <* char ':' <* newline
      S.fromList <$> number `sepBy` singleSpace


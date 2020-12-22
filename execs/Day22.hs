{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-|
   Name: Crab Combat
   Url: <https://adventofcode.com/2020/day/22>
-}

module Day22 (main) where

import Advent
import Control.Arrow ((&&&))
import Control.Monad.State
import Control.Lens
import qualified Data.Sequence as S
import qualified Data.Set as SS
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char
import Data.Monoid
import Data.Maybe

type Input  = (Deck, Deck)
type Output = Int

type Deck = S.Seq Int
data Player = Player1 | Player2
data GameState = Combat
  { _player1 :: Deck
  , _player2 :: Deck
  , _history :: SS.Set (Deck, Deck)
  , _gameOver :: Maybe Player
  }
$(makeLenses ''GameState)

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedInput 22 parseInput

part1 :: Input -> Output
part1 = ans . uncurry combat
part2 :: Input -> Output
part2 = ans . fst . uncurry recursiveCombat

loop round = do
  p1 <- use player1
  p2 <- use player2
  checkWinByHistory p1 p2 >>= \case
    Just Player1 -> return p1
    Just Player2 -> return p2
    Nothing -> do
      t1 <- drawCard player1
      t2 <- drawCard player2
      round t1 t2 >>= putToBottom t1 t2
      checkGameOver >>= \case
        Just Player1 -> use player1
        Just Player2 -> use player2
        Nothing      -> loop round

playRound t1 t2 = do
  l1 <- uses player1 S.length
  l2 <- uses player2 S.length
  if t1 <= l1 && t2 <= l2
     then do
       p1' <- uses player1 (S.take t1)
       p2' <- uses player2 (S.take t2)
       let p1Mx = maximum p1'
           p2Mx = maximum p2'
       if p1Mx > p2Mx && p1Mx > (t1+t2)
          then return . Just $ Player1
          else return $ recursiveCombat p1' p2' ^. (_2 . gameOver)
     else playRoundHigher t1 t2

playRoundHigher t1 t2
  | t1 > t2   = return . Just $ Player1
  | otherwise = return . Just $ Player2

checkGameOver = do
  p1 <- use player1
  p2 <- use player2
  case (p1, p2) of
    (Empty, _) -> gameOver <#= Just Player2
    (_, Empty) -> gameOver <#= Just Player1
    _          -> gameOver <#= Nothing

putToBottom p1 p2 (Just Player1) = player1 %= \d -> d |> p1 |> p2
putToBottom p1 p2 (Just Player2) = player2 %= \d -> d |> p2 |> p1

drawCard :: Lens' GameState Deck -> StateT GameState Maybe Int
drawCard player = do
  p :< ps <- use player
  player #= ps
  return p

checkWinByHistory p1 p2 = do
  seen <- uses history (SS.member (p1,p2))
  history %= SS.insert (p1,p2)
  if seen then gameOver <#= Just Player1 else gameOver <#= Nothing

initialState p1 p2 = Combat p1 p2 mempty Nothing

combat p1 p2 = fromJust . evalStateT (loop playRoundHigher) $ initialState p1 p2
recursiveCombat p1 p2 = fromJust . runStateT (loop playRound) $ initialState p1 p2

ans = getSum . S.foldMapWithIndex (\i a -> Sum ((i+1) * a)) . S.reverse

-- | Parsing
parseInput :: Parser Input
parseInput = (,) <$> parsePlayer <* newline <* newline <*> parsePlayer
  where
    parsePlayer = do
      symbol "Player" <* number <* char ':' <* newline
      S.fromList <$> number `sepBy` singleSpace


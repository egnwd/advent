{-# LANGUAGE TemplateHaskell #-}
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
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char
import Data.Monoid

type Input  = (Deck, Deck)
type Output = Int

type Deck = S.Seq Int

data GameState = Combat
  { _player1 :: Deck
  , _player2 :: Deck
  , _gameOver :: Maybe Int
  }
$(makeLenses ''GameState)

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedInput 22 parseInput


-- | Parsing
parseInput :: Parser Input
parseInput = (,) <$> parsePlayer <* newline <* newline <*> parsePlayer
  where
    parsePlayer = do
      symbol "Player" <* number <* char ':' <* newline
      S.fromList <$> number `sepBy` singleSpace

loop :: StateT GameState Maybe Deck
loop = do
  p1 S.:< p1s <- uses player1 S.viewl
  p2 S.:< p2s <- uses player2 S.viewl
  player1 #= p1s
  player2 #= p2s
  if p1 > p2
     then player1 %= \d -> d |> p1 |> p2
     else player2 %= \d -> d |> p2 |> p1
  checkGameOver >>= \case
    Nothing -> loop
    Just 1  -> use player1
    Just 2  -> use player2

checkGameOver = do
  p1 <- use player1
  p2 <- use player2
  if S.null p1
     then gameOver <#= Just 2
     else if S.null p2
       then gameOver <#= Just 1
       else gameOver <#= Nothing

part1 (p1, p2) = ans <$> evalStateT loop (Combat p1 p2 Nothing)

ans = getSum . S.foldMapWithIndex (\i a -> Sum ((i+1) * a)) . S.reverse

part2 :: Input -> Output
part2 = const 0

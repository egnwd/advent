{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.

module AOC.Challenge.Day06 (
    day06a
  , day06b
  ) where

import AOC.Solver
import AOC.Common
import Linear.V2
import Text.Megaparsec (try)
import Text.Megaparsec.Char
import Data.Array.Base (UArray)
import Data.Array.IArray (IArray, elems)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.ST (STUArray, runSTUArray)
import Control.Monad.ST (ST)
import Control.Lens.Getter
import Data.Ix
import Control.Monad
import Control.Applicative

data OpCode = TurnOn | TurnOff | Toggle deriving (Show, Eq)

data Instruction = I { op :: OpCode, box :: V2 Point } deriving (Show, Eq)

parseOp :: CharParser OpCode
parseOp = try (TurnOn <$ string "turn on") <|> (TurnOff <$ string "turn off") <|> (Toggle <$ string "toggle")

class IArray UArray a => Unboxed a where
  zero :: a
  newSTUArray :: Ix i => (i, i) -> a -> ST s (STUArray s i a)
  readSTUArray :: Ix i => STUArray s i a -> i -> ST s a
  writeSTUArray :: Ix i => STUArray s i a -> i -> a -> ST s ()

instance Unboxed Int where
  zero = 0
  newSTUArray = newArray
  readSTUArray = readArray
  writeSTUArray = writeArray

instance Unboxed Bool where
  zero = False
  newSTUArray = newArray
  readSTUArray = readArray
  writeSTUArray = writeArray

parse :: CharParser Instruction
parse = do
  op <- pTok parseOp
  mn <- V2 <$> pDecimal <* char ',' <*> pDecimal
  string " through "
  mx <- V2 <$> pDecimal <* char ',' <*> pDecimal
  pure $ I op (V2 mn mx)

eval :: Unboxed m => (OpCode -> m -> m) -> [Instruction] -> UArray Point m
eval ops is = runSTUArray $ do
  a <- newSTUArray (pure 0, pure 999) zero
  forM_ is $ \I { op, box=(V2 mn mx) } ->
    forM_ [V2 x y | x <- [mn ^. _x..mx ^. _x], y <- [mn ^. _y..mx ^. _y]] $ \pos -> do
      val <- readSTUArray a pos
      writeSTUArray a pos (ops op val)
  return a

lights :: OpCode -> Bool -> Bool
lights TurnOn  = const True
lights TurnOff = const False
lights Toggle  = not

brightness :: OpCode -> Int -> Int
brightness TurnOn  = (+1)
brightness TurnOff = \i -> max (i-1) 0
brightness Toggle  = (+2)

day06a :: [Instruction] :~> Int
day06a = MkSol
  { sParse = parseLines parse
  , sShow  = show
  , sSolve = Just . countTrue id . elems . eval lights
  }

day06b :: [Instruction] :~> Int
day06b = MkSol
  { sParse = parseLines parse
  , sShow  = show
  , sSolve = Just . sum . elems . eval brightness
  }

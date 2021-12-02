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
import Linear              (V2(..))
import Control.Applicative ((<|>))
import Data.Monoid         (Sum(..))

data Submarine = Sub { loc :: Point, aim :: Int } deriving Show

instance Semigroup Submarine where
    (Sub v a) <> (Sub (V2 x' y') a') = Sub (v + V2 x' (y' + x' * a)) (a + a')

instance Monoid Submarine where
    mempty = Sub (pure 0) 0

parser :: CharParser (Sum Point)
parser = do
  dir <- pTok $
        (Sum . flip V2 0     <$ "forward")
    <|> (Sum . V2 0 . negate <$ "up")
    <|> (Sum . V2 0          <$ "down")
  dir <$> pDecimal

parser' :: CharParser Submarine
parser' = do
    dir <- pTok $
          ((\x -> Sub (V2 x 0) 0) <$ "forward")
      <|> (Sub (pure 0) . negate  <$ "up")
      <|> (Sub (pure 0)           <$ "down")
    dir <$> pDecimal

day02a :: [Sum Point] :~> Int
day02a = MkSol
    { sParse = parseLines parser
    , sShow  = show
    , sSolve = Just . product . getSum . mconcat
    }

day02b :: [Submarine] :~> Int
day02b = MkSol
    { sParse = parseLines parser'
    , sShow  = show
    , sSolve = Just . product . loc . mconcat
    }

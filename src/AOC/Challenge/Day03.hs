{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import AOC.Solver
import AOC.Common (CharParser, parseMaybeLenient, odds, evens)
import Text.Megaparsec.Char
import Control.Monad.Combinators ((<|>), many)
import qualified Data.Set as S
import Linear.V2
import Control.Lens.TH
import Control.Monad.State
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.Lens

data Compass = North | East | South | West deriving (Eq, Ord, Show)

go :: Compass -> V2 Int
go North = V2 1    0
go East  = V2 0    1
go South = V2 (-1) 0
go West  = V2 0    (-1)

parseDirection :: CharParser Compass
parseDirection =
      (North <$ char '^')
  <|> (East  <$ char '>')
  <|> (South <$ char 'v')
  <|> (West  <$ char '<')

data Globe = Globe
  { _curPos :: V2 Int
  , _visited :: S.Set (V2 Int)
  }

$(makeLenses ''Globe)

solve :: Compass -> State Globe ()
solve c = do
  new <- curPos <+= go c
  visited %= S.insert new

startGlobe :: Globe
startGlobe = Globe (V2 0 0) (S.singleton (V2 0 0))

parta :: [Compass] -> Int
parta cs = views visited S.size $ execState (mapM solve cs) startGlobe

partb :: [Compass] -> Int
partb cs = S.size $ S.union santaGlobe roboGlobe
  where
    santaGlobe = view visited $ execState (mapM solve (odds cs)) startGlobe
    roboGlobe  = view visited $ execState (mapM solve (evens cs)) startGlobe

day03a :: _ :~> _
day03a = MkSol
    { sParse = parseMaybeLenient (many parseDirection)
    , sShow  = show
    , sSolve = Just . parta
    }

day03b :: _ :~> _
day03b = MkSol
    { sParse = parseMaybeLenient (many parseDirection)
    , sShow  = show
    , sSolve = Just . partb
    }

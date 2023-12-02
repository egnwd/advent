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


import AOC.Solver ((:~>)(..))
import AOC.Common (CharParser, pTok, pDecimal, parseLines, (&&&))
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Control.Monad (guard)
import Control.Applicative ((<|>))
import qualified Data.Map                       as M
import qualified Text.Megaparsec                as P

data Colour = Red | Blue | Green deriving (Eq, Ord, Show)

data Game = Game { gId :: Int
                 , gCubeSets :: Map Colour Int
                 } deriving (Eq, Show)

gameParser :: CharParser Game
gameParser = Game <$> idParser <*> maxSetsParser
    where
        idParser = "Game " *> pDecimal <* ": "
        cubeParser = Green <$ "green" <|> Red <$ "red" <|> Blue <$ "blue"
        setParser = (flip (,) <$> pTok pDecimal <*> cubeParser) `P.sepBy` (pTok ",")
        maxSetsParser = M.unionsWith max <$> (M.fromList <$> setParser) `P.sepBy` (pTok ";")

isLegal :: Map Colour Int -> Bool
isLegal = isPossible Blue 14 &&& isPossible Green 13 &&& isPossible Red 12
    where
        isPossible c n cs = M.lookup c cs <= Just n

day02a :: [Game] :~> Int
day02a = MkSol
    { sParse = parseLines gameParser
    , sShow  = show
    , sSolve = Just . sum . mapMaybe (\(Game i cs) -> i <$ guard (isLegal cs))
    }

day02b :: [Game] :~> Int
day02b = MkSol
    { sParse = parseLines gameParser
    , sShow  = show
    , sSolve = Just . sum . map (product . gCubeSets)
    }

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
import AOC.Common (CharParser, pTok, pDecimal, parseLines)
import Data.Map (Map)
import Data.Monoid (Sum(..))
import Control.Applicative ((<|>))
import qualified Data.Map                       as M
import qualified Text.Megaparsec                as P

data Colour = Red | Blue | Green deriving (Eq, Ord, Show)

data Game = Game { gId :: Int
                 , gCubeSets :: [[(Colour, Int)]]
                 } deriving (Eq, Show)

gameParser :: CharParser Game
gameParser = Game <$> ("Game " *> pDecimal <* ": ") <*> (setParser `P.sepBy` (pTok ",") `P.sepBy` (pTok ";"))
    where
        cubeParser = pTok $ Green <$ "green" <|> Red <$ "red" <|> Blue <$ "blue"
        setParser = flip (,) <$> pTok pDecimal <*> cubeParser

isPossible :: (Foldable t, Ord a, Ord k) => k -> a -> t (Map k a) -> Bool
isPossible c n = all (\cs -> M.lookup c cs <= Just n)

day02a :: _ :~> _
day02a = MkSol
    { sParse = parseLines gameParser
    , sShow  = show
    , sSolve = Just
        . getSum
        . foldMap (Sum . fst)
        . filter (isPossible Blue 14 . snd)
        . filter (isPossible Green 13 . snd)
        . filter (isPossible Red 12 . snd)
        . map (\(Game i cs) -> (i, map (M.fromListWith (+)) cs))
    }

day02b :: _ :~> _
day02b = MkSol
    { sParse = parseLines gameParser
    , sShow  = show
    , sSolve = Just . sum . map (product . M.fromListWith max . concat . gCubeSets)
    }

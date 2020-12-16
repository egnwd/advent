{-# LANGUAGE TypeSynonymInstances #-}
{-|
   Name: Seating System
   Url: <https://adventofcode.com/2020/day/11>
-}

module Day11 (main) where

import Advent
import Prelude hiding (unlines)

import Control.Arrow ((&&&))
import Control.Monad

import Text.Megaparsec ((<|>), many)
import Text.Megaparsec.Char (char)
import Data.Vector (Vector(..), fromList, (!?), imap)
import qualified Data.Vector as V

main :: IO ()
main = (print . (part1 &&& part2)) . fromList . join =<< getParsedLines 11 parseInput

data Seat = Floor | Occupied | EmptySeat deriving (Eq)
type Input   = Vector Seat
type Output  = Int

instance Show Seat where
  show Floor = "."
  show Occupied = "#"
  show EmptySeat = "L"

-- | Parsing
parseInput :: Parser [Seat]
parseInput = many parseTerrain

parseTerrain :: Parser Seat
parseTerrain = Floor <$ char '.' <|> Occupied <$ char '#' <|> EmptySeat <$ char 'L'

width = 99
height = 95

part1 :: Input -> Output
part1 = countOccupied . fix (runSeats newSeat applyDiff)

part2 :: Input -> Output
part2 = countOccupied . fix (runSeats newSeat' applyDiff')

countOccupied = V.length . V.filter (==Occupied)

runSeats new check ss = imap (runSeat new check ss) ss
runSeat new check ss ix s = new s (adjSeats check ix ss)

newSeat EmptySeat 0 = Occupied
newSeat Occupied n
  | n >= 4    = EmptySeat
  | otherwise = Occupied
newSeat s _ = s

newSeat' EmptySeat 0 = Occupied
newSeat' Occupied n
  | n >= 5    = EmptySeat
  | otherwise = Occupied
newSeat' s _ = s

toIx (i,j)
  | i < 0 || j < 0 || i >= height || j >= width = -1
  | otherwise = i*width+j

fromIx ix = ix `divMod` width

adjSeats f ix ss = let (i,j) = fromIx ix
                  in sum $ getNeighbours f (i,j) ss

getNeighbours f ix ss = [f ix ds ss | ds <- [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1,-1), (1,0), (1,1)]]

applyDiff (i,j) (di,dj) ss = let (i',j') = (i+di, j+dj)
                                 s = ss !? toIx (i',j')
                              in case s of
                                   Just Occupied -> 1
                                   _ -> 0

applyDiff' (i,j) ds@(di,dj) ss = let (i',j') = (i+di, j+dj)
                                     s = ss !? toIx (i',j')
                                  in case s of
                                      Nothing -> 0
                                      Just Floor -> applyDiff (i',j') ds ss
                                      Just Occupied -> 1
                                      Just EmptySeat -> 0

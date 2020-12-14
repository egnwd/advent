{-|
   Name: Docking Data
   Url: <https://adventofcode.com/2020/day/14>
-}

module Day14 (main) where

import Advent
import Prelude hiding (unlines)
import Control.Arrow ((&&&))
import Data.Bits
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Lens
import Data.Foldable

import Debug.Trace

import qualified Data.Map as M

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedInput 14 parseInput

type Memory = M.Map Int Int
data DockingData = Dock
  { andMask, orMask :: Int
  , mem :: Memory
  } deriving Show

type Input  = [DockingData]
type Output = Int

parseInput :: Parser [DockingData]
parseInput = parseData `sepBy` newline <* eof

parseData :: Parser DockingData
parseData = do
  tmpMask <- symbol "mask = " *> parseTmpMask
  let (andMask, orMask) = convertMask tmpMask
  mem <- parseMemory

  return $ Dock andMask orMask mem

parseTmpMask :: Parser [Maybe Int]
parseTmpMask = many p <* newline
  where p = (Just . digitToInt <$> binDigitChar) <|> (Nothing <$ char 'X')

parseMemory :: Parser Memory
parseMemory = parseMemory' $ M.empty

parseMemory' :: Memory -> Parser Memory
parseMemory' v = do
  i <- fromIntegral <$> (symbol "mem" *> between "[" "]" number <* symbol " = ")
  num <- fromIntegral <$> number
  let v' = v & at i ?~ num
  (try (newline *> parseMemory' v')) <|> return v'

convertMask bits = (andMask, orMask)
  where
    initialAnd = foldr' (\i x -> setBit x i) zeroBits [0..35] :: Int
    initialOr = zeroBits :: Int
    andMask = ifoldl (updateAnd) initialAnd bits
    orMask  = ifoldl updateOr initialOr bits
    updateAnd _ m Nothing  = m
    updateAnd _ m (Just 1) = m
    updateAnd i m (Just 0) = clearBit m (35-i)
    updateOr _ m Nothing  = m
    updateOr _ m (Just 0) = m
    updateOr i m (Just 1) = setBit m (35-i)

part1 :: Input -> Output
part1 ds = sum . M.unions . reverse . map f $ ds
  where
    f :: DockingData -> Memory
    f dock = M.map (\x -> (x .&. andMask dock) .|. orMask dock) (mem dock)

part2 :: Input -> Output
part2 = const 0

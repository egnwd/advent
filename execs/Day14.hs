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

import qualified Data.Map as M

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedInput 14 parseInput

type SetInstr = [(Int, Int)]
type Memory = M.Map Int Int
type Mask = [Maybe Int]
data DockingData = Dock
  { tmpMask :: Mask
  , mem :: SetInstr
  } deriving Show

type Input  = [DockingData]
type Output = Int

parseInput :: Parser [DockingData]
parseInput = parseData `sepBy` newline <* eof

parseData :: Parser DockingData
parseData = do
  tmpMask <- symbol "mask = " *> parseTmpMask <* newline
  mem <- parseMemory

  return $ Dock tmpMask mem

parseTmpMask :: Parser [Maybe Int]
parseTmpMask = many p
  where p = (Just . digitToInt <$> binDigitChar) <|> (Nothing <$ char 'X')

parseMemory :: Parser SetInstr
parseMemory = reverse <$> parseMemory' mempty

parseMemory' :: SetInstr -> Parser SetInstr
parseMemory' v = do
  i <- fromIntegral <$> (symbol "mem" *> between "[" "]" number <* symbol " = ")
  num <- fromIntegral <$> number
  let v' = (i,num) : v
  (try (newline *> parseMemory' v')) <|> return v'

andMask bits = ifoldl (updateAnd) initialAnd (reverse bits)
  where
    initialAnd = foldr' (\i x -> setBit x i) zeroBits [0..length bits] :: Int
    updateAnd _ m Nothing  = m
    updateAnd _ m (Just 1) = m
    updateAnd i m (Just 0) = clearBit m i

orMask bits = ifoldl updateOr initialOr (reverse bits)
  where
    initialOr = zeroBits :: Int
    updateOr _ m Nothing  = m
    updateOr _ m (Just 0) = m
    updateOr i m (Just 1) = setBit m i

part1 :: Input -> Output
part1 ds = gather . map updateMemory $ ds
  where
    updateMemory :: DockingData -> Memory
    updateMemory dock = M.fromList $ fmap (\(i,x) -> (i, (x .&. and dock) .|. or dock)) (mem dock)
    and = andMask . tmpMask
    or  = orMask . tmpMask

part2 :: Input -> Output
part2 ds = gather . map updateMemory $ ds

updateMemory :: DockingData -> Memory
updateMemory dock = foldl f M.empty memory
  where
    memory = mem dock
    mask = tmpMask dock
    -- f m[addr] = x
    f m (addr, x) = foldr (\a m' -> m' & at a ?~ x) m $ makeMasks addr mask

makeMasks addr = addressSpace . addressMask addr

addressSpace :: Mask -> [Int]
addressSpace = ifoldl createAddresses [zeroBits] . reverse
  where
    createAddresses i as (Just 1) = map (flip setBit i) as
    createAddresses i as (Just 0) = map (flip clearBit i) as
    createAddresses i as Nothing  = concatMap (\x -> [setBit x i, clearBit x i]) as

addressMask :: Int -> Mask -> Mask
addressMask addr = reverse . imap (\i -> fmap (updateMask i)) . reverse
  where
    updateMask idx 0 = if testBit addr idx then 1 else 0
    updateMask _ 1 = 1

showMask :: Mask -> String
showMask = map f
  where
    f (Just 1) = '1'
    f (Just 0) = '0'
    f Nothing  = 'X'

gather = sum . M.unions . reverse

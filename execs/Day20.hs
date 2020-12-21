{-# LANGUAGE DeriveFunctor #-}
{-|
   Name: Jurassic Jigsaw
   Url: <https://adventofcode.com/2020/day/20>
-}

module Day20 (main) where

import Advent
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad
import Data.Ix (range)
import Data.List
import Data.Maybe
import Text.Megaparsec ((<|>), some, sepBy)
import Text.Megaparsec.Char
import qualified Data.IntMap as M
import qualified Data.Map as MP
import qualified Data.Set as S

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedInput 20 parseInput

newtype Tile a = Tile { unTile :: a } deriving (Eq, Ord, Functor)
type PhotoTile = Tile [[Bool]]
type Pixels = Tile [Bool]
type Input  = M.IntMap PhotoTile
type Output = Int
type Photo = MP.Map (Int,Int) Int

instance Show PhotoTile where
  show = unTile . fmap (unlines . map (map showPixel))

instance Show Pixels where
  show = unTile . (fmap . fmap) showPixel

showPixel True = '#'
showPixel False = '.'

maxSize = 12-1

parseInput :: Parser Input
parseInput = M.fromList <$> parseTile `sepBy` (newline <* newline)

parseTile :: Parser (Int, PhotoTile)
parseTile = (,) <$> (symbol "Tile" *> number <* char ':' <* newline) <*> parseTile'
  where parseTile' = Tile <$> some (True <$ char '#' <|> False <$ char '.') `sepBy` singleSpace

part1 tiles = let photo = fromJust $ assembleMap tiles
               in MP.foldrWithKey (\k a acc -> if k `S.member` corners then a * acc else acc) 1 photo

corners = let x = maxSize in S.fromList [(0,0), (0,x), (x,0), (x,x)]

part2 :: Input -> Output
part2 = const 0

assembleMap :: Input -> Maybe Photo
assembleMap tiles = listToMaybe $ assembleMap' allOptions (range ((0,0),(maxSize,maxSize))) MP.empty
  where
    allOptions = allMutations tiles
    assembleMap' _    []     photo = return $ fst <$> photo
    assembleMap' opts (i:is) photo = do
      key <- MP.keys opts
      guard $ valid allOptions (opts MP.! key) i photo
      let opts' = opts MP.\\ MP.mapKeys (fst key,) mutations
      let photo' = MP.insert i key photo
      assembleMap' opts' is photo'

-- solve :: MP.Map (Int, Int) PhotoTile -> [(Int, Int)] -> MP.Map (Int,Int) (Int,Int) -> [Photo]

valid :: MP.Map (Int, Int) PhotoTile -> PhotoTile -> (Int, Int) -> MP.Map (Int, Int) (Int, Int) -> Bool
valid _      _    (0,0) _ = True
valid lookup tile (0,j) photo = (lookup MP.! (photo MP.! (0,j-1))) `isLeftOf` tile
valid lookup tile (i,0) photo = (lookup MP.! (photo MP.! (i-1,0))) `isAbove` tile
valid lookup tile (i,j) photo = (lookup MP.! (photo MP.! (i-1,j))) `isAbove` tile && (lookup MP.! (photo MP.! (i,j-1))) `isLeftOf` tile

t `isAbove`  t' = t ^. bottomRow == t' ^. topRow
t `isLeftOf` t' = t ^. rightColumn == t' ^. leftColumn

-- Mutations (Rotations and Reflections) and the ability to apply them to tagged Tiles (i.e. (Int, Tile)
allMutations = MP.fromList . concatMap mutate . M.toList
mutate (key, tile) = MP.elems $ MP.mapWithKey (\i m -> ((key,i),m tile)) mutations
mutations = MP.fromList $ zip [0..] [ f . g | f <- reflections, g <- rotations ]

rotations :: [PhotoTile -> PhotoTile]
rotations = [id, rotl, rotl . rotl, rotr]

rotl = fmap $ reverse . transpose
rotr = fmap $ transpose . reverse

reflections :: [PhotoTile -> PhotoTile]
reflections = [id, flipH, flipV]

flipH = fmap reverse
flipV = (fmap . fmap) reverse

-- Lenses to fetch edges
topRow :: Getter PhotoTile Pixels
topRow = to (fmap head)

bottomRow :: Getter PhotoTile Pixels
bottomRow = to (fmap last)

leftColumn :: Getter PhotoTile Pixels
leftColumn = to ((fmap . fmap) head)

rightColumn :: Getter PhotoTile Pixels
rightColumn = to ((fmap .fmap) last)

{-|
   Name: Jurassic Jigsaw
   Url: <https://adventofcode.com/2020/day/20>
-}

module Day20 (main) where

import Advent
import Advent.Compass
import Control.Arrow ((&&&))
import qualified Data.IntMap as M
import Text.Megaparsec ((<|>), some, sepBy)
import Text.Megaparsec.Char
import Data.List
import Control.Lens
import Data.Maybe
import Control.Monad
import Data.Bifunctor
import qualified Data.Set as S
import Data.Monoid
import qualified Data.Vector as V

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedInput 20 parseInput

newtype Tile a = Tile { unTile :: a } deriving (Eq, Ord)
type PhotoTile = Tile [[Bool]]
type Pixels = Tile [Bool]
type Input  = M.IntMap PhotoTile
type Output = Int

instance Show PhotoTile where
  show = unTile . fmap (unlines . map (map showPixel))

instance Show Pixels where
  show = unTile . (fmap . fmap) showPixel

showPixel True = '#'
showPixel False = '.'

instance Functor Tile where
  fmap f (Tile xs) = Tile $ f xs

parseInput :: Parser Input
parseInput = M.fromList <$> parseTile `sepBy` (newline <* newline)

parseTile :: Parser (Int, PhotoTile)
parseTile = (,) <$> (symbol "Tile" *> number <* char ':' <* newline) <*> parseTile'
  where parseTile' = Tile <$> some (True <$ char '#' <|> False <$ char '.') `sepBy` singleSpace

part1 = ans . solve

part2 :: Input -> Output
part2 = const 0

edges :: [(Int, PhotoTile) -> (Int, Pixels)]
edges = map (second . view) [topRow, bottomRow, leftColumn, rightColumn]

allEdges :: (Int, PhotoTile) -> [(Int, Pixels)]
allEdges t = S.toList . S.fromList $ edges <*> mutate t

solve input = let edges = concatMap allEdges (M.toList input)
               in M.fromListWith (+) $ do
                 (id1, e1) <- edges
                 (id2, e2) <- edges
                 guard $ id1 /= id2
                 guard $ e1 == e2
                 return (id1, 1)

ans = getProduct . foldMap (Product . fst) . take 4 . sortOn snd . M.toList

-- Mutations (Rotations and Reflections) and the ability to apply them to tagged Tiles (i.e. (Int, Tile)
mutate tile = map (`second` tile) mutations
mutations = [ f . g | f <- reflections, g <- rotations ]

rotations :: [PhotoTile -> PhotoTile]
rotations = [id, rotl, rotl . rotl, rotr]

rotl = fmap $ reverse . transpose
rotr = fmap $ transpose . reverse

reflections :: [PhotoTile -> PhotoTile]
reflections = [id, flipH, flipV, flipH . flipV]

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

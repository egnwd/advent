{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-|
   Name: Jurassic Jigsaw
   Url: <https://adventofcode.com/2020/day/20>
-}

module Day20 (main) where

import Advent
import Control.Arrow ((&&&))
import Control.Monad
import Control.Lens
import Data.Ix (range)
import Data.List (transpose)
import Data.Semigroup
import Linear.V2
import Data.Maybe
import Text.Megaparsec ((<|>), some, sepBy, parseMaybe, notFollowedBy, try)
import Text.Megaparsec.Char
import qualified Data.IntMap as M
import Data.Map ((!), (\\))
import qualified Data.Map as MP
import qualified Data.Set as S

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedInput 20 parseInput

newtype Tile a = Tile { unTile :: a } deriving (Eq, Ord, Functor, Num)
type PhotoTile = Tile [[Bool]]
type Pixels = Tile [Bool]
type Input  = M.IntMap PhotoTile
type Output = Int
type Photo = MP.Map (V2 Int) (Int, PhotoTile)

instance Show PhotoTile where
  show = unTile . fmap (unlines . map (map showPixel))

instance Show Pixels where
  show = unTile . (fmap . fmap) showPixel

instance Applicative Tile where
  pure    = Tile
  f <*> a = Tile (unTile f $ unTile a)

showPixel True  = '#'
showPixel False = '.'

maxSize = 12-1

parseInput :: Parser Input
parseInput = M.fromList <$> parseTile `sepBy` (newline <* newline)

parseTile :: Parser (Int, PhotoTile)
parseTile = (,) <$> (symbol "Tile" *> number <* char ':' <* newline) <*> parseTile'
parseTile' = Tile <$> some (True <$ char '#' <|> False <$ (char '.' <|> char ' ')) `sepBy` try (newline >> notFollowedBy newline)

part1 :: Input -> Output
part1 tiles = let photo = fromJust $ assembleMap tiles
               in MP.foldrWithKey (\k a acc -> if k `S.member` corners then fst a * acc else acc) 1 photo

corners = S.fromList [V2 0 0, V2 0 maxSize, V2 maxSize 0, V2 maxSize maxSize]

part2 :: Input -> Output
part2 tiles = let photo = fromJust $ assembleMap tiles
                  stitchedPhoto = createPhoto photo
                  mx = getMax . foldMap (findMonsters . fmap convert) $ mutations <*> [stitchedPhoto]
               in unTile $ (S.size . convert <$> stitchedPhoto) - pure mx * (S.size <$> seaMonster)

createPhoto :: Photo -> PhotoTile
createPhoto photo = let stiched = stitchRows . stitchColumns $ noBorders in stiched ! 0
                    where
                      noBorders = MP.map (fmap shave . snd) photo
                      stitchColumns = MP.mapKeysWith mergeColumns (view _y)
                      stitchRows = MP.mapKeysWith mergeRows (const 0)
                      shave = map (tail . init) . tail . init
                      mergeRows r l = zipWith (++) <$> l <*> r
                      mergeColumns b t = (++) <$> t <*> b

assembleMap :: Input -> Maybe Photo
assembleMap tiles = listToMaybe $ assembleMap' allOptions (range (pure 0, pure maxSize)) MP.empty
  where
    allOptions = allMutations tiles
    assembleMap' _    []     photo = return $ (\k@(id,_) -> (id, allOptions ! k)) <$> photo
    assembleMap' opts (i:is) photo = do
      key <- MP.keys opts                                   -- Take next available option
      guard $ valid allOptions (opts ! key) i photo         -- Check if the available option is possible
      let opts' = opts \\ MP.mapKeys (fst key,) mutationMap -- Remove tile from map of options
      let photo' = MP.insert i key photo                    -- Add mutated tile to the photo
      assembleMap' opts' is photo'                          -- Recurse onto the next slot in the photo

valid :: MP.Map (Int, Int) PhotoTile -> PhotoTile -> V2 Int -> MP.Map (V2 Int) (Int, Int) -> Bool
valid _      _      (V2 0 0) _     = True
valid lookup tile v@(V2 0 _) photo = previous lookup photo (left v) `isLeftOf` tile
valid lookup tile v@(V2 _ 0) photo = previous lookup photo (up v) `isAbove` tile
valid lookup tile v          photo = previous lookup photo (up v) `isAbove` tile && previous lookup photo (left v) `isLeftOf` tile

left v = v - V2 0 1
up v = v - V2 1 0
previous a b = (a !) . (b !)

t `isAbove`  t' = bottomRow t == topRow t'
t `isLeftOf` t' = rightColumn t == leftColumn t'

-- Mutations (Rotations and Reflections) and the ability to apply them to tagged Tiles (i.e. (Int, Tile)

allMutations = MP.fromList . concatMap mutate . M.toList
mutate (key, tile) = MP.elems $ MP.mapWithKey (\i m -> ((key,i),m tile)) mutationMap
mutationMap = MP.fromList $ zip [0..] mutations
mutations = [ f . g | f <- reflections, g <- rotations ]

rotations :: [PhotoTile -> PhotoTile]
rotations = [id, rotl, rotl . rotl, rotr]

rotl = fmap $ reverse . transpose
rotr = fmap $ transpose . reverse

reflections :: [PhotoTile -> PhotoTile]
reflections = [id, flipH, flipV]

flipH = fmap reverse
flipV = (fmap . fmap) reverse

topRow      = fmap head
bottomRow   = fmap last
leftColumn  = (fmap . fmap) head
rightColumn = (fmap .fmap) last

-- | Sea Monster

sm = fromJust . parseMaybe parseTile' $
  "                  # \n\
  \#    ##    ##    ###\n\
  \ #  #  #  #  #  #   "

seaMonster :: Tile (S.Set (V2 Int))
seaMonster = convert <$> sm

convert grid = S.fromList [V2 i j | (i, rs) <- zip [0..] grid, (j, c) <- zip [0..] rs, c]

totalPixels = 8 * 12

findMonsters :: Tile (S.Set (V2 Int)) -> Max Int
findMonsters photo = Max . length . filter f $ range (pure 0, pure totalPixels)
  where
    f :: V2 Int -> Bool
    f dv = let currMonster = S.map (+dv) <$> seaMonster
               overlap = S.intersection <$> currMonster <*> photo
            in currMonster == overlap

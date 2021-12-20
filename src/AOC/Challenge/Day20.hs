{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day20
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 20.

module AOC.Challenge.Day20 (
    day20a
  , day20b
  ) where

import           AOC.Solver        ((:~>)(..))
import           AOC.Common        (parseAsciiMap, countTrue, Point, boundingBox)
import           Control.Lens      ((^.))
import           Data.IntMap       (IntMap)
import           Data.Map.NonEmpty (NEMap)
import           Data.Traversable  (forM)
import           Linear.V2         (_yx, V2(V2))
import qualified Data.IntMap as IM
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Text as T

data Pixel = Dark | Light deriving (Eq, Ord, Enum)
type Image = NEMap Point Pixel
type EnhanceAlgorithm = IntMap Pixel

instance Show Pixel where
    show Light = "#"
    show Dark = "."

lightOrDark :: Char -> Maybe Pixel
lightOrDark '#' = Just Light
lightOrDark '.' = Just Dark
lightOrDark _ = Nothing

parseEnhanceAlgorithm :: String -> Maybe EnhanceAlgorithm
parseEnhanceAlgorithm = fmap (IM.fromList . zip [0..]) . traverse lightOrDark

parseInitialImage :: String -> Maybe Image
parseInitialImage = NEM.nonEmptyMap . parseAsciiMap lightOrDark

parse :: [String] -> Maybe (EnhanceAlgorithm, Image)
parse [a,b]
  = case (parseEnhanceAlgorithm a, parseInitialImage b) of
        (Just a', Just b') -> Just (a',b')
        _                  -> Nothing
parse _     = Nothing

binToDec :: [Pixel] -> Int
binToDec = foldl (\a -> (+) (2*a) . fromEnum) 0

solve :: Int -> EnhanceAlgorithm -> Image -> Image
solve n enhanceAlg img0 = snd . (!! n) $ iterate (\(p, mp) -> (flipPixel p, enhance p enhanceAlg mp)) (Dark, img0)
    where
        flipPixel Dark  = enhanceAlg IM.! 0
        flipPixel Light = enhanceAlg IM.! 511

enhance :: Pixel -> EnhanceAlgorithm -> Image -> Image
enhance p alg img0 = img1
    where
        V2 (V2 mnx mxx) (V2 mny mxy) = sequence $ boundingBox (NEM.keys img0)
        keys = NE.fromList [ V2 x y | x <- [mnx-1..mxx+1], y <- [mny-1..mxy+1] ]
        Just img1 = fmap NEM.fromList . forM keys $ \k -> do
            let n = binToDec [NEM.findWithDefault p (k + (k' ^. _yx)) img0 | k' <- sequence (pure [-1, 0, 1]) :: [Point]]
            (k,) <$> IM.lookup n alg

day20 :: Int -> (EnhanceAlgorithm, Image) :~> Int
day20 n = MkSol
    { sParse = parse . map T.unpack . T.splitOn "\n\n" . T.pack
    , sShow  = show
    , sSolve = Just . countTrue (==Light) . uncurry (solve n)
    }

day20a :: (EnhanceAlgorithm, Image) :~> Int
day20a = day20 2

day20b :: (EnhanceAlgorithm, Image) :~> Int
day20b = day20 50

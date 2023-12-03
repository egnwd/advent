-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import           AOC.Solver    ((:~>)(..))
import           AOC.Common    (parseMaybeLenient, parseAsciiMap, Point, allNeighboursSet)
import           Control.Monad (guard)
import           Data.Maybe    (mapMaybe)
import           Data.Char     (isNumber)
import           Control.Lens  (makePrisms, sumOf)
import           Linear        (V2(..))
import           Control.Arrow ((&&&))
import           Data.Map      (Map)
import qualified Data.Map                       as M
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char.Lexer     as PP

type PartLocation = ([Point], Int)
data Gear = PartialGear Int | FullGear Int
makePrisms ''Gear

parseNumberMap :: String -> (Map Point Char, [PartLocation])
parseNumberMap = parseAsciiMap symbolGetter &&& go
    where
        symbolGetter '.' = Nothing
        symbolGetter a
          | isNumber a = Nothing
          | otherwise = Just a
        go = concat
           . concat
           . sequenceA
           . zipWith (parseMaybeLenient . parseLine) [0..]
           . lines
        parseLine y = P.many $ P.try $ do
            P.takeWhileP Nothing (not . isNumber)
            start <- P.unPos . P.sourceColumn <$> P.getSourcePos
            n <- PP.decimal
            end <- P.unPos . P.sourceColumn <$> P.getSourcePos
            return ([V2 (x-1) y | x <- [start..end-1]], n)

nearSymbols :: (Char -> Bool) -> Map Point Char -> [Point] -> Map Point Char
nearSymbols p symbols = M.restrictKeys (M.filter p symbols) . foldMap allNeighboursSet

findPartNumbers :: Map Point Char -> [PartLocation] -> [Int]
findPartNumbers symbolMap = mapMaybe (\(k, a) -> a <$ guard (hasSymbolNearby k))
    where
        hasSymbolNearby = not . M.null . nearSymbols (const True) symbolMap

findGearRatios :: Map Point Char -> [PartLocation] -> Map Point Gear
findGearRatios symbolMap = foldl partToGear M.empty
    where
        partToGear mp (k,a) = foldr (M.alter (tweakGear a)) mp . nearbyGearPositions $ k
        nearbyGearPositions = M.keys . nearSymbols (== '*') symbolMap
        tweakGear a Nothing = Just . PartialGear $ a
        tweakGear a (Just (PartialGear b)) = Just . FullGear $ a * b
        tweakGear _ _ = Nothing

day03a :: (Map Point Char, [PartLocation]) :~> Int
day03a = MkSol
    { sParse = Just . parseNumberMap
    , sShow  = show
    , sSolve = Just . sum . uncurry findPartNumbers
    }

day03b :: (Map Point Char, [PartLocation]) :~> Int
day03b = MkSol
    { sParse = Just . parseNumberMap
    , sShow  = show
    , sSolve = Just . sumOf (traverse . _FullGear) . uncurry findGearRatios
    }

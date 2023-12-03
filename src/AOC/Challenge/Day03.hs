{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import           AOC.Prelude hiding (indexed, (&&&))

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP
import Control.Lens
import Control.Lens.Indexed
import Data.Map.Lens (toMapOf)
import Linear
import Control.Arrow ((***), (&&&))
import Control.Lens.TH (makePrisms)

type PartLocation = ([Point], Int)
data Gear = PartialGear Int | FullGear Int
makePrisms ''Gear

parseNumberMap :: String -> (Map Point Char, _)
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

day03a :: (Map Point Char, _) :~> _
day03a = MkSol
    { sParse = Just . parseNumberMap
    , sShow  = show
    , sSolve = Just . sum . uncurry findPartNumbers
    }

day03b :: _ :~> _
day03b = MkSol
    { sParse = sParse day03a
    , sShow  = show
    , sSolve = Just . sumOf (traverse . _FullGear) . uncurry findGearRatios
    }

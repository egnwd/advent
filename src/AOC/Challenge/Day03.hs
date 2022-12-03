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

import           AOC.Common (splitHalf, singleItem, charFinite, Letter)
import           AOC.Solver ((:~>)(..))
import           Control.Lens (preview)
import           Control.Monad ((<=<))
import           Data.Finite (Finite, getFinite, weakenN)
import           Data.List.Split (chunksOf)
import           Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty              as NES
import qualified AOC.Common.Set.NonEmpty        as NES

-- | 53 because [1..52] U [0]
type Item = Finite 53
type Rucksack = NESet Item

toPresent :: Char -> Maybe Item
toPresent = fmap (uncurry priority) . charFinite

priority :: Bool -> Letter -> Item
priority True = (+27) . weakenN
priority False = (+1) . weakenN

findCommonItem :: (Rucksack, Rucksack) -> Maybe Item
findCommonItem = preview singleItem . uncurry NES.intersection

findGroupBadge :: [Rucksack] -> Maybe Item
findGroupBadge = preview singleItem <=< NES.intersections

sumBy :: (Traversable t, Applicative f) => (a -> f (Finite n)) -> t a -> f Integer
sumBy f = fmap sum . traverse (fmap getFinite . f)

day03a :: [(Rucksack, Rucksack)] :~> _
day03a = MkSol
    { sParse = traverse (splitHalf <=< traverse toPresent) . lines
    , sShow  = show
    , sSolve = sumBy findCommonItem
    }

day03b :: [[Rucksack]] :~> _
day03b = MkSol
    { sParse = fmap (chunksOf 3) . traverse (NES.toNonEmptySet <=< traverse toPresent) . lines
    , sShow  = show
    , sSolve = sumBy findGroupBadge
    }

module AOC.Common.Set.NonEmpty
  ( toNonEmptySet
  , intersections
  ) where

import           Safe (foldl1May)
import           Data.Set (Set)
import           Data.Set.NonEmpty (NESet)
import qualified Data.Set                   as S
import qualified Data.Set.NonEmpty          as NES

toNonEmptySet :: Ord a => [a] -> Maybe (NES.NESet a)
toNonEmptySet = NES.nonEmptySet . S.fromList

intersections :: Ord a => [NESet a] -> Maybe (Set a)
intersections = foldl1May S.intersection . fmap NES.toSet

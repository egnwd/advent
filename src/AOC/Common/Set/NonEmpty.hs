module AOC.Common.Set.NonEmpty
  ( toNonEmptySet
  , intersections
  ) where

import           Control.Monad (foldM)
import           Data.Set.NonEmpty (NESet)
import qualified Data.Set                   as S
import qualified Data.Set.NonEmpty          as NES

toNonEmptySet :: Ord a => [a] -> Maybe (NES.NESet a)
toNonEmptySet = NES.nonEmptySet . S.fromList

intersections :: (Traversable t, Ord a) => NESet a -> t (NESet a) -> Maybe (NESet a)
intersections = foldM (curry (NES.nonEmptySet . uncurry NES.intersection))

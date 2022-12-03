module AOC.Common.Set.NonEmpty
  ( toNonEmptySet
  , intersections
  ) where

import           Control.Lens (uncons)
import           Control.Monad (foldM, (<=<))
import           Data.Set.NonEmpty (NESet)
import qualified Data.Set                   as S
import qualified Data.Set.NonEmpty          as NES

toNonEmptySet :: Ord a => [a] -> Maybe (NES.NESet a)
toNonEmptySet = NES.nonEmptySet . S.fromList

intersections :: Ord a => [NESet a] -> Maybe (NESet a)
intersections = uncurry (foldM (\s1 s2 -> NES.nonEmptySet $ NES.intersection s1 s2)) <=< uncons

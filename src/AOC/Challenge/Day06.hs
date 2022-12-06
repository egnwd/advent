-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day06 (
    day06a
  , day06b
                           , interceptStream
  ) where

import           AOC.Solver    ((:~>)(..))
import           AOC.Common    (indexed')
import           Data.Set      (Set, size, fromList)
import           Data.Conduino (Pipe, (.|), runPipe, await)
import           Data.Foldable (toList)
import qualified Data.Conduino.Combinators as C

toSet :: (Traversable t, Ord a) => t a -> Set a
toSet = fromList . toList

findPacket :: (Monad m, Ord a) => Int -> Pipe a c v m (Maybe (Set a))
findPacket n
  = C.consecutive n
  .| C.map toSet
  .| C.filter ((== n) . size)
  .| await

interceptStream :: (Foldable t, Num a, Ord b) => Int -> t b -> Maybe a
interceptStream n s = runPipe $ C.sourceList s .| indexed' (findPacket n)

day06 :: Int -> String :~> Int
day06 n = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = interceptStream n
    }

day06a :: String :~> Int
day06a = day06 4

day06b :: String :~> Int
day06b = day06 14

-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.

module AOC.Challenge.Day04 (
    day04a
  , day04b
  ) where

import           AOC.Common (countTrue)
import           AOC.Solver ((:~>) (..))
import Control.Monad ((<=<))
import           Data.Function (on)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set.NonEmpty   as S
import qualified Data.List.NonEmpty   as NE

type Password = NonEmpty Char
type Passphrase = NonEmpty Password
type Passphrases = NonEmpty Passphrase

parsePassphrases :: String -> Maybe Passphrases
parsePassphrases = NE.nonEmpty <=< traverse (NE.nonEmpty <=< traverse NE.nonEmpty . words) . lines

sameSize :: Foldable f => f a -> f a -> Bool
sameSize = (==) `on` length

dedupe :: Ord a => NonEmpty a -> NonEmpty a
dedupe = S.toList . S.fromList

dedupeAnagrams :: Passphrase -> Passphrase
dedupeAnagrams = dedupe . fmap dedupe

day04a :: Passphrases :~> Int
day04a = MkSol
    { sParse = parsePassphrases
    , sShow  = show
    , sSolve = Just . countTrue (sameSize <*> dedupe)
    }

day04b :: Passphrases :~> Int
day04b = MkSol
    { sParse = parsePassphrases
    , sShow  = show
    , sSolve = Just . countTrue (sameSize <*> dedupeAnagrams)
    }

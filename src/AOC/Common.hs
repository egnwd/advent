-- |
-- Module      : AOC.Challenge
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Meant to be a place to include common functionality used across
-- different parts in the challenge.
--


module AOC.Common (
                    CharParser(..)
                  , Parser(..)
                  , pSpace
                  , pWord
                  , parseMaybeLenient
                  , parseOrFail
                  , parseLines
                  , fixedPoint
                  , freqs
                  , lookupFreq
                  ) where

import Data.Char
import Data.Foldable
import Data.Map (Map)
import Data.Maybe
import Data.Void
import AOC.Util
import qualified Data.Map             as M
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P
import qualified Data.Text            as T

type CharParser = P.Parsec Void String
type Parser = P.Parsec Void T.Text

pSpace :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s ()
pSpace = P.skipMany (P.char ' ')

-- | Doesn't fail if the whole imput is not consumed
parseMaybeLenient :: P.Parsec Void s a -> s -> Maybe a
parseMaybeLenient p = eitherToMaybe . P.parse p "parseMaybeLenient"

parseOrFail :: (P.Stream s, P.ShowErrorComponent e) => P.Parsec e s a -> s -> a
parseOrFail p = either (error . P.errorBundlePretty) id . P.parse p "parseOrFail"

pTok :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s a -> P.Parsec e s a
pTok p = p <* pSpace

pWord :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s String
pWord = pTok $ P.many (P.satisfy (not . isSpace))

parseLines :: P.Parsec Void String a -> String -> Maybe [a]
parseLines p = Just . mapMaybe (parseMaybeLenient p) . lines

-- | Repeat a function until you get the same result twice in a row
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f = go
  where
    go !x
        | x == y    = x
        | otherwise = go y
      where
        y = f x

-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList

-- | Look up a count from a frequency map, defaulting to zero if item is
-- not found
lookupFreq :: Ord a => a -> Map a Int -> Int
lookupFreq = M.findWithDefault 0

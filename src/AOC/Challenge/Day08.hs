{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.

module AOC.Challenge.Day08 (
    day08a
  , day08b
  ) where

import AOC.Solver ((:~>)(..))
import AOC.Common (CharParser, parseLines)
import Text.Megaparsec.Char (char, hexDigitChar, string)
import Text.Megaparsec (try, noneOf)
import Control.Monad.Combinators (many, choice, between, (<|>))
import Data.Functor (($>))
import Data.Monoid (getSum)


quoted :: CharParser a -> CharParser a
quoted = between (char '"') (char '"')

parseDecode :: CharParser Int
parseDecode = sum <$> quoted (many (choice [pNormal, pEscaped, pHex]))

pNormal :: CharParser Int
pNormal = 1 <$ noneOf "\\\""

pEscaped :: CharParser Int
pEscaped = 1 <$ (try (string "\\\\") <|> try (string "\\\""))

pHex :: CharParser Int
pHex = try (string "\\x") *> hexDigitChar *> hexDigitChar $> 1

size :: String -> Int
size = length . concat . lines

decode :: String -> Maybe Int
decode = fmap sum . parseLines parseDecode

encode :: String -> Maybe Int
encode = Just . getSum . foldMap ((2 <>) . foldMap encode') . lines
    where
        encode' '"'  = 2
        encode' '\\' = 2
        encode' _    = 1

day08a :: String :~> Int
day08a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = \a -> (`subtract` size a) <$> decode a
    }

day08b :: String :~> Int
day08b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = \a -> subtract (size a) <$> encode a
    }

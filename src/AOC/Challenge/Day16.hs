{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

-- |
-- Module      : AOC.Challenge.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.

module AOC.Challenge.Day16 (
    day16a
  , day16b
  ) where

import AOC.Solver               ((:~>)(..))
import AOC.Common               (hexToBin, parseMaybeLenient, parseOrFail, CharParser)
import Control.Lens             (preview)
import Control.Monad            ((<=<))
import Data.Finite              (Finite, finite)
import Data.Functor.Foldable    (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Maybe               (fromMaybe)
import Numeric.Lens             (binary)
import Text.Megaparsec          (anySingle, takeP, count, getOffset, setOffset, many)

data PacketValue a = L Integer | Op [a] deriving (Show, Eq, Functor, Traversable, Foldable)

data Packet = Packet
    { _pVersion :: !Int
    , _pId :: !(Finite 8)
    , _pValue :: PacketValue Packet
    } deriving (Show, Eq)
makeBaseFunctor ''Packet

toBinOrZero :: (Integral a) => String -> a
toBinOrZero = fromMaybe 0 . preview binary

parsePacket :: CharParser Packet
parsePacket = do
    v   <- toBinOrZero <$> takeP (Just "Version") 3
    typ <- finite . toBinOrZero <$> takeP (Just "Operator") 3
    val <- case typ of
        4 -> L <$> parseLiteral
        _ -> Op <$> parseOperator
    pure $ Packet v typ val

parseLiteral :: CharParser Integer
parseLiteral = getOffset >>= fmap toBinOrZero . parseLiteral'
    where
        parseLiteral' :: Int -> CharParser String
        parseLiteral' start = do
            f <- anySingle
            d <- takeP (Just "Literal") 4
            rest <- if f == '0' then pure [] else parseLiteral' start
            pure (d++rest)

parseOperator :: CharParser [Packet]
parseOperator = do
    i <- anySingle
    if i == '0' then parse15Operator else parse11Operator

    where
        parse15Operator :: CharParser [Packet]
        parse15Operator = do
            len <- toBinOrZero <$> takeP (Just "Length of subpackets") 15
            subpackets <- parseOrFail (many parsePacket) <$> takeP (Just "subpackets") len
            getOffset >>= setOffset . (+len)
            pure subpackets

        parse11Operator :: CharParser [Packet]
        parse11Operator = takeP (Just "Count of subpackets") 11 >>= (`count` parsePacket) . toBinOrZero

getVersionSum :: PacketF Int -> Int
getVersionSum (PacketF v _ (L _)) = v
getVersionSum (PacketF v _ (Op ps)) = v + sum ps

calculate :: PacketF Integer -> Integer
calculate (PacketF _  0 (Op ps))    = sum ps
calculate (PacketF _  1 (Op ps))    = product ps
calculate (PacketF _  2 (Op ps))    = minimum ps
calculate (PacketF _  3 (Op ps))    = maximum ps
calculate (PacketF _  4 (L  l))     = l
calculate (PacketF _  5 (Op [a,b])) = if a >  b then 1 else 0
calculate (PacketF _  6 (Op [a,b])) = if a <  b then 1 else 0
calculate (PacketF _  7 (Op [a,b])) = if a == b then 1 else 0
calculate _                         = error "Invalid calculation"


day16a :: Packet :~> Int
day16a = MkSol
    { sParse = parseMaybeLenient parsePacket <=< hexToBin
    , sShow  = show
    , sSolve = Just . cata getVersionSum
    }

day16b :: Packet :~> Integer
day16b = MkSol
    { sParse = parseMaybeLenient parsePacket <=< hexToBin
    , sShow  = show
    , sSolve = Just . cata calculate
    }

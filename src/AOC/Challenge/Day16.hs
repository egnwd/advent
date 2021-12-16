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
import Control.Lens             (preview, (<&>))
import Control.Monad            ((<=<))
import Data.Functor.Foldable    (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Maybe               (fromMaybe)
import Numeric.Lens             (binary)
import Text.Megaparsec          (anySingle, takeP, count, many, (<|>), failure, ErrorItem(Tokens))
import Text.Megaparsec.Char     (char)
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE

data Packet
    = Literal !Int Integer
    | Operator !Int Operator [Packet]

data Operator
    = OpSum
    | OpProd
    | OpMin
    | OpMax
    | OpGT
    | OpLT
    | OpEQ

makeBaseFunctor ''Packet

-- ^ Parsing

toBinOrZero :: (Integral a) => String -> a
toBinOrZero = fromMaybe 0 . preview binary

toOp :: Int -> CharParser (Maybe Operator)
toOp = \case
    0 -> pure $ Just OpSum
    1 -> pure $ Just OpProd
    2 -> pure $ Just OpMin
    3 -> pure $ Just OpMax
    4 -> pure   Nothing
    5 -> pure $ Just OpGT
    6 -> pure $ Just OpLT
    7 -> pure $ Just OpEQ
    n -> failure (Tokens <$> NE.nonEmpty (show n)) (S.singleton (Tokens . NE.fromList $ "Packet Type (0-7)"))

parsePacket :: CharParser Packet
parsePacket = do
    v   <- toBinOrZero <$> takeP (Just "Version") 3
    typ <- toOp . toBinOrZero =<< takeP (Just "Operator") 3
    case typ of
      Nothing -> Literal v <$> parseLiteral
      Just op -> Operator v op <$> parseOperator

parseLiteral :: CharParser Integer
parseLiteral = toBinOrZero <$> parseLiteral'
    where
        parseLiteral' :: CharParser String
        parseLiteral' = do
            f <- anySingle
            d <- takeP (Just "Literal") 4
            rest <- if f == '0' then pure [] else parseLiteral'
            pure (d++rest)

parseOperator :: CharParser [Packet]
parseOperator = (char '0' *> parse15Operator) <|> (char '1' *> parse11Operator)
    where
        parse15Operator = takeP (Just "Length of subpackets") 15 >>= (takeP (Just "subpackets") . toBinOrZero) <&> parseOrFail (many parsePacket)
        parse11Operator = takeP (Just "Count of subpackets") 11 >>= (`count` parsePacket) . toBinOrZero

-- ^ Main Functions

getVersionSum :: PacketF Int -> Int
getVersionSum (LiteralF v _) = v
getVersionSum (OperatorF v _ ps) = v + sum ps

calculate :: PacketF Integer -> Integer
calculate (LiteralF _ l)     = l
calculate (OperatorF _ op ps) = getOp op ps
    where
        binOp f = \case
            [a,b] -> if a `f` b then 1 else 0
            _     -> 0
        getOp = \case
            OpSum  -> sum
            OpProd -> product
            OpMin  -> minimum
            OpMax  -> maximum
            OpGT   -> binOp (>)
            OpLT   -> binOp (<)
            OpEQ   -> binOp (==)

-- ^ Solutions

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

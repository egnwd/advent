{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day16 (
    day16a
  , day16b
  ) where

import           AOC.Prelude hiding (many)
import Numeric.Lens
import Control.Lens
import Data.Finite
import Text.Megaparsec

data PacketType = Literal Integer | Operator Integer deriving (Show, Eq)
data PacketValue = L Integer | Op [Packet] deriving (Show, Eq)
data Packet = Packet { _pVersion :: Integer, _pId :: PacketType, _pValue :: PacketValue } deriving (Show, Eq)

getVersionSum (Packet v _ (L _)) = Sum v
getVersionSum (Packet v _ (Op ps)) = Sum v <> foldMap getVersionSum ps

calculate (Packet _ (Operator 0) (Op ps)) = sum $ map calculate ps
calculate (Packet _ (Operator 1) (Op ps)) = product $ map calculate ps
calculate (Packet _ (Operator 2) (Op ps)) = minimum $ map calculate ps
calculate (Packet _ (Operator 3) (Op ps)) = maximum $ map calculate ps
calculate (Packet _ (Literal 4)  (L l))    = l
calculate (Packet _ (Operator 5) (Op [a,b])) = if calculate a >  calculate b then 1 else 0
calculate (Packet _ (Operator 6) (Op [a,b])) = if calculate a <  calculate b then 1 else 0
calculate (Packet _ (Operator 7) (Op [a,b])) = if calculate a == calculate b then 1 else 0

toBinOrZero :: (Integral a) => String -> a
toBinOrZero = fromMaybe 0 . preview binary

parsePacket :: CharParser Packet
parsePacket = do
    v   <- toBinOrZero <$> takeP (Just "Version") 3
    typ <- (Literal . toBinOrZero <$> "100") <|> (Operator . toBinOrZero <$> takeP (Just "Operator") 3)
    val <- case typ of
        Literal _  -> L <$> parseLiteral
        Operator _ -> Op <$> parseOperator
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

hexToBin :: Finite 16 -> Maybe [Finite 2]
hexToBin = traverse packFinite <=< toBinDigits
    where
        toBinDigits = \case
          0  -> pure [0,0,0,0]
          1  -> pure [0,0,0,1]
          2  -> pure [0,0,1,0]
          3  -> pure [0,0,1,1]
          4  -> pure [0,1,0,0]
          5  -> pure [0,1,0,1]
          6  -> pure [0,1,1,0]
          7  -> pure [0,1,1,1]
          8  -> pure [1,0,0,0]
          9  -> pure [1,0,0,1]
          10 -> pure [1,0,1,0]
          11 -> pure [1,0,1,1]
          12 -> pure [1,1,0,0]
          13 -> pure [1,1,0,1]
          14 -> pure [1,1,1,0]
          15 -> pure [1,1,1,1]
          _  -> Nothing

hexToBinStr :: String -> Maybe String
hexToBinStr = fmap (map (review binDigit) . concat) . traverse hexToBin <=< traverse (preview hexDigit)

day16a :: _ :~> _
day16a = MkSol
    { sParse = parseMaybeLenient parsePacket <=< hexToBinStr
    , sShow  = show
    , sSolve = Just . getSum . getVersionSum
    }

day16b :: _ :~> _
day16b = MkSol
    { sParse = parseMaybeLenient parsePacket <=< hexToBinStr
    , sShow  = show
    , sSolve = Just . calculate
    }

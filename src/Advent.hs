{-# LANGUAGE RankNTypes #-}
module Advent
    ( getRawInput
    , getParsedInput
    , getParsedLines
    , getParsedDoubleLines
    , parseLines
    , parseDoubleLines
    , Parser (..)
    , number
    , symbol
    , passportParsers
    , parsePassportField
    , singleSpace
    , fix
    ) where

import Advent.Passport
import Prelude hiding (readFile, lines)
import Data.Text hiding (empty, map)
import Data.Text.IO (readFile)
import System.FilePath
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Lens hiding ((<.>))
import Data.Finite
import Data.Char
import Refined

type Parser = Parsec Void Text

getRawInput :: Int -> IO Text
getRawInput i = readFile ("inputs" </> "input" <> show i <.> "txt")

getParsedInput :: Int -> Parser a -> IO a
getParsedInput i p = do
  input <- getRawInput i
  case parse p "input" input of
    Left e -> fail (errorBundlePretty e)
    Right a -> return a

getParsedLines :: Int -> Parser a -> IO [a]
getParsedLines i p = do
  input <- getRawInput i
  either fail return (parseLines p input)

getParsedDoubleLines :: (Show a) => Int -> Parser a -> IO [a]
getParsedDoubleLines i p = do
  input <- getRawInput i
  either fail return (parseDoubleLines p input)

parseLines :: Parser a -> Text -> Either String [a]
parseLines p input =
  case parse (traverse parse' $ lines input) "input" input of
    Left  e -> Left (errorBundlePretty e)
    Right a -> Right a
  where parse' x = setInput x *> p <* eof <* setInput "\n" <* newline

parseDoubleLines :: (Show a) => Parser a -> Text -> Either String [a]
parseDoubleLines p input =
  case parse (p `sepBy` (newline >> newline) <* eof) "input" input of
    Left  e -> Left (errorBundlePretty e)
    Right a -> Right a

sc      = L.space hspace1 empty empty
lexeme :: Parser a -> Parser a
lexeme  = L.lexeme sc

symbol  = L.symbol sc
integer = lexeme L.decimal
number  = L.signed sc integer

-- Passport Parsing
parseKey k = try (string k) <* char ':'
ident = pack <$> many (char '#' <|> alphaNumChar)

parsePassportField :: Parser UnvalidatedPassport
parsePassportField = parseField' "byr" byr
         <|> parseField' "iyr" iyr
         <|> parseField' "eyr" eyr
         <|> parseField' "hgt" hgt
         <|> parseField' "hcl" hcl
         <|> parseField' "ecl" ecl
         <|> parseField' "pid" pid
         <|> mempty <$ (parseKey "cid" *> ident)
           where
             parseField' :: Text -> UnvalidatedSetter a -> (Parser UnvalidatedPassport)
             parseField' k s = parseKey k *> ident >>= setValue s
             setValue s = return . flip (set s) mempty . Const . pure

passportParsers = Passport
  { _byr = fromIntegral <$> number >>= refineFail
  , _iyr = fromIntegral <$> number >>= refineFail
  , _eyr = fromIntegral <$> number >>= refineFail
  , _hgt = parseHeight
  , _hcl = parseHair
  , _ecl = parseEyeColor
  , _pid = parsePid
  }

parseEyeColor =
      AMB <$ p "amb"
  <|> BLU <$ p "blu"
  <|> BRN <$ p "brn"
  <|> GRY <$ p "gry"
  <|> GRN <$ p "grn"
  <|> HZL <$ p "hzl"
  <|> OTH <$ p "oth"
    where p = try . string

parseHeight = (HIn <$> try parseIn) <|> (HCm <$> parseCm)
  where
    parseIn = fromIntegral <$> number <* string "in" >>= refineFail
    parseCm = fromIntegral <$> number <* string "cm" >>= refineFail

parseHair = (map (finite . fromIntegral . digitToInt)) . unpack <$> (char '#' *> takeWhileP Nothing isHexDigit) >>= refineFail
parsePid = (map (finite . fromIntegral . digitToInt)) . unpack <$> takeWhileP Nothing isDigit >>= refineFail

singleSpace :: Parser ()
singleSpace = try (spaceChar >> notFollowedBy spaceChar)

-- Utilities
fix f x
  | x == fx = fx
  | otherwise = fix f fx
  where fx = f x


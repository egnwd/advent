module Advent
    ( getRawInput
    , getParsedInput
    , getParsedLines
    , parseLines
    , Parser (..)
    , number
    , symbol
    ) where

import Prelude hiding (readFile, lines)
import Data.Text hiding (empty)
import Data.Text.IO (readFile)
import System.FilePath
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L

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

parseLines :: Parser a -> Text -> Either String [a]
parseLines p input =
  case parse (traverse parse' $ lines input) "input" input of
    Left  e -> Left (errorBundlePretty e)
    Right a -> Right a
  where parse' x = setInput x *> p <* eof <* setInput "\n" <* newline

sc      = L.space hspace1 empty empty
lexeme :: Parser a -> Parser a
lexeme  = L.lexeme sc

symbol  = L.symbol sc
integer = lexeme L.decimal
number  = L.signed sc integer

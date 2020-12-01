{-# LANGUAGE OverloadedStrings #-}
module Advent
    ( getParsedLines
    , Parser (..)
    , number
    ) where

import Prelude hiding (readFile)
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

getParsedLines :: Int -> Parser a -> IO [a]
getParsedLines i p = do
  input <- getRawInput i
  either fail return (parseLines p input)

parseLines :: Parser a -> Text -> Either String [a]
parseLines p input =
  case parse ((p `sepBy` newline) <* eof) "input" input of
    Left  e -> Left (errorBundlePretty e)
    Right a -> Right a

sc      = L.space hspace1 empty empty
lexeme :: Parser a -> Parser a
lexeme  = L.lexeme sc
integer = lexeme L.decimal
number  = L.signed sc integer

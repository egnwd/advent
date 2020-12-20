{-|
   Name: Monster Messages
   Url: <https://adventofcode.com/2020/day/19>
-}

module Day19 (main) where

import Advent
import Control.Arrow ((&&&))
import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Char
import Data.Maybe
import Text.Megaparsec (takeWhile1P, try, sepBy, parseMaybe, eof, some)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.IntMap ((!))
import qualified Data.IntMap as M
import qualified Data.Text as T

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedInput 19 parseInput

data Rule = C !Char | Then Rule Rule | Or Rule Rule | Val !Int
type Rules  = M.IntMap Rule
type Messages = [T.Text]
type Input  = (Rules, Messages)

parseInput :: Parser Input
parseInput = (,) <$> parseRules <* (newline <* newline) <*> parseMessages
  where parseMessages = takeWhile1P Nothing isAlpha `sepBy` newline

parseRules :: Parser Rules
parseRules = M.unions <$> parseRule `sepBy` singleSpace
  where
    parseRule = M.singleton <$> number <* symbol ":" <*> expr
    expr = makeExprParser term table
    table = [[InfixL (Then <$ hspace)], [InfixL (Or <$ (char '|' >> hspace))]]
    term = (Val <$> L.decimal <|> C <$> (char '"' *> letterChar <* char '"')) <* hspace

part1 (rules, messages) = match (eval rules (rules ! 0)) messages
part2 (rules, messages) = match p messages
  where
    p = do
        n <- some (try $ eval rules (rules ! 42))
        m <- some (eval rules (rules ! 31))
        if length n > length m then return () else fail "bad message"

match p = length . mapMaybe (parseMaybe (p <* eof))

eval :: Rules -> Rule -> Parser ()
eval rules (Then l r) = eval rules l >> eval rules r
eval rules (Or l r)   = try (eval rules l) <|> eval rules r
eval rules (Val i)    = eval rules (rules ! i)
eval _     (C c)      = void $ char c

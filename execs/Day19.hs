{-|
   Name: Monster Messages
   Url: <https://adventofcode.com/2020/day/19>
-}

module Day19 (main) where

import Advent
import Control.Arrow ((&&&))
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Char
import Text.Megaparsec (takeWhile1P, try, choice, sepBy)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.IntMap ((!))
import qualified Data.IntMap as M
import qualified Data.Text as T

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedInput 19 parseInput

data Rule = C !Char | Then Rule Rule | Or Rule Rule | Val !Int
type Rules  = M.IntMap Rule
type Messages = [String]
type Input  = (Rules, Messages)

parseInput :: Parser Input
parseInput = (,) <$> parseRules <* (newline <* newline) <*> parseMessages

parseRules :: Parser Rules
parseRules = M.unions <$> parseRule `sepBy` singleSpace
  where
    parseRule = M.singleton <$> number <* symbol ":" <*> parseSubRule

parseSubRule = choice [C <$> parseChar, parseTop]
parseChar = try (char '"' *> letterChar <* char '"')

parseTop = do
  left <- parseMiddle
  go left
    where
      go acc = (" | " >> parseMiddle >>= go . Or acc) <|> return acc

parseMiddle = do
    left <- parseBottom
    go left
  where
    go acc = try (" " >> parseBottom >>= go . Then acc) <|> return acc

parseBottom = Val <$> L.decimal

parseMessages = map T.unpack <$> takeWhile1P Nothing isAlpha `sepBy` newline

part1 (rules, messages) = match rules messages
part2 (rules, messages) = let rules' = sandwich 11 . repeat1 8 $ rules
                           in match rules' messages

repeat1 = M.updateWithKey (\k r -> Just $ r `Or` (r `Then` Val k))
sandwich = M.updateWithKey (\k t@(Then l r) -> Just $ t `Or` ((l `Then` Val k) `Then` r))

match rs = length . filter (any null . flip runReader rs . eval (rs ! 0))

eval :: Rule -> String -> Reader (M.IntMap Rule) Messages
eval (Then l r) = (fmap concat . traverse (eval r)) <=< eval l
eval (Or l r)   = \s -> eval l s +++ eval r s
eval (Val i)    = (asks (!i) >>=) . flip eval
eval (C c)      = \case
                      (c':s') | c == c' -> return [s']
                      _ -> return []

(+++) :: (Applicative f) => f [a] -> f [a] -> f [a]
(+++) = liftA2 (++)

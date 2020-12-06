{-# LANGUAGE OverloadedStrings #-}
{-|
   Name: Corrupt Passwords
   Url: <https://adventofcode.com/2020/day/2>
-}

module Day02 (main) where

import Prelude hiding (unlines)
import Advent
import Text.Megaparsec (many)
import Text.Megaparsec.Char (letterChar)
import Control.Applicative (liftA3)
import qualified Data.Text as T

-- $setup
-- >>> let parse = Data.Either.fromRight undefined . Advent.parseLines parseInput
-- >>> :set -XOverloadedStrings

main :: IO ()
main = do
  input <- getParsedLines 2 parseInput
  print $ countCorrectPasswords isCorrect input
  print $ countCorrectPasswords isCorrectNew input

data PasswordPolicy = Policy !Integer !Integer !Char
type Password = T.Text
type Input = (PasswordPolicy, Password)

type Validator = PasswordPolicy -> Password -> Bool


-- | Parsing
parseInput :: Parser Input
parseInput = (,) <$> (parsePasswordPolicy <* symbol ":") <*> parsePassword

parsePasswordPolicy :: Parser PasswordPolicy
parsePasswordPolicy = liftA3 Policy (number <* symbol "-") number letterChar

parsePassword  :: Parser Password
parsePassword = T.pack <$> many letterChar

-- | Count the number of valid passwords according to the password policy
--
-- >>> let check v = countCorrectPasswords v . parse
-- >>> check isCorrect "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"
-- 2
--
-- >>> check isCorrectNew "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"
-- 1
countCorrectPasswords :: Validator -> [Input] -> Int
countCorrectPasswords v = foldl (\acc x -> if uncurry v x then acc + 1 else acc) 0

-- | Checks if a password is valid, according to the *old* specification
isCorrect :: PasswordPolicy -> Password -> Bool
isCorrect (Policy l u _) "" = l <= 0 && u >= 0
isCorrect pol@(Policy l u c) p
  | c == T.head p = isCorrect (Policy (l-1) (u-1) c) (T.tail p)
  | otherwise   = isCorrect pol (T.tail p)

-- | Checks if a password is valid, according to the *new* specification
isCorrectNew :: PasswordPolicy -> Password -> Bool
isCorrectNew (Policy l u c) p = (T.index p (fromIntegral l-1) == c) /= (T.index p (fromIntegral u-1) == c)

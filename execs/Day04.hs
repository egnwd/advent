{-# LANGUAGE OverloadedStrings, GADTs #-}
{-|
   Name: Passport Processing
   Url: <https://adventofcode.com/2020/day/4>
-}

module Main
  ( main
  ) where

import Advent
import Prelude hiding (unlines)
import Data.Maybe
import Text.Megaparsec  hiding (count, between)
import Text.Megaparsec.Char hiding (count)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec as MP
import Data.Map hiding (filter, drop, map)

import qualified Data.Text as T

-- $setup
-- >>> let parse = Data.Either.fromRight undefined . Advent.parseLines parseInput
-- >>> :set -XOverloadedStrings

main :: IO ()
main = do
  input <- getParsedDoubleLines 4 parseInput
  print $ validatePassports isValid input
  print $ validatePassports isValid' input

type Input    = [Passport]
type Passport = Map T.Text (Maybe T.Text)
data Passport' =
  Passport' { _byr :: Int
           , _iyr :: Int
           , _eyr :: Int
           , _hgt :: String
           , _hcl :: String
           , _ecl :: String
           , _pid :: String
           , _cid :: Maybe Int
           }
data Unit = CM | IN

instance Show Unit where
  show CM = "cm"
  show IN = "in"

type Output = Int

showt :: Show a => a -> T.Text
showt = T.pack . show

-- | Parsing
parseInput :: Parser Passport
parseInput = fromList <$> parseField `sepBy` (try $ spaceChar <* notFollowedBy spaceChar)

parseField :: Parser (T.Text, Maybe T.Text)
parseField = choice [parseBirthYear, parseIssueYear, parseExpiryYear, parseHeight, parseHairColour, parseEyeColour, parsePassportId, parseCountryId]

parseKey :: T.Text -> Parser T.Text
parseKey k = try (string k) <* char ':'

-- | parseBirthYear
-- >>> parseTest parseBirthYear "byr:2002"
-- ("byr",Just "2002")
--
-- >>> parseTest parseBirthYear "byr:2003"
-- ("byr",Nothing)
parseBirthYear = do
  key <- parseKey "byr"
  value <- L.decimal
  if between 1920 value 2002
     then return $ (key, Just $ showt value)
     else return $ (key, Nothing)

-- | parseIssueYear
-- >>> parseTest parseIssueYear "iyr:2012"
-- ("iyr",Just "2012")
--
-- >>> parseTest parseIssueYear "iyr:2050"
-- ("iyr",Nothing)
parseIssueYear = do
  key <- parseKey "iyr"
  value <- L.decimal
  if between 2010 value 2020
     then return $ (key, Just $ showt value)
     else return $ (key, Nothing)

-- | parseExpiryYear
-- >>> parseTest parseExpiryYear "eyr:2022"
-- ("eyr",Just "2022")
--
-- >>> parseTest parseExpiryYear "eyr:2018"
-- ("eyr",Nothing)
parseExpiryYear = do
  key <- parseKey "eyr"
  value <- L.decimal
  if between 2020 value 2030
     then return $ (key, Just $ showt value)
     else return $ (key, Nothing)

parseHeight = do
  key <- parseKey "hgt"
  value <- L.decimal
  unit <- optional $ CM <$ string "cm" <|> IN <$ string "in"
  let validRange CM = if between 150 value 193
                         then Just . T.pack $ show value ++ "cm"
                         else Nothing
      validRange IN = if between 59 value 76
                         then Just . T.pack $ show value ++ "in"
                         else Nothing

  case unit of
    Just u -> return (key, validRange u)
    Nothing -> return (key, Nothing)

parseHairColour :: Parser (T.Text, Maybe T.Text)
parseHairColour = do
  key <- parseKey "hcl"
  value <- consume $ char '#' *> MP.count 6 hexDigitChar

  return $ (key, T.pack . ('#' :) . show <$> value)

parseEyeColour :: Parser (T.Text, Maybe T.Text)
parseEyeColour = do
  key <- parseKey "ecl"
  value <- consume $ choice (map (try . string) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

  return $ (key, showt <$> value)

parsePassportId :: Parser (T.Text, Maybe T.Text)
parsePassportId = do
  key <- parseKey "pid"
  value <- consume $ try (MP.count 9 digitChar <* notFollowedBy digitChar)

  return $ (key, showt <$> value)

parseCountryId = do
  key <- parseKey "cid"
  value <- T.pack <$> many alphaNumChar

  return $ (key, Just $ showt value)

consume p = (Just <$> p) <|> (Nothing <$ many (char '#' <|> alphaNumChar))

--
-- >>> :{
--  let ps = parse $ "\
--  \ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\
--  \byr:1937 iyr:2017 cid:147 hgt:183cm\
--  \\
--  \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\
--  \hcl:#cfa07d byr:1929\
--  \\
--  \hcl:#ae17e1 iyr:2013\
--  \eyr:2024\
--  \ecl:brn pid:760753108 byr:1931\
--  \hgt:179cm\
--  \\
--  \hcl:#cfa07d eyr:2025 pid:166559648\
--  \iyr:2011 ecl:brn hgt:59in"
-- :}
-- >>> validatePassports ps
-- 2
validatePassports :: (Passport -> Bool) -> Input -> Output
validatePassports v = count v

-- | isValid passport with optional country code
-- >>> isValid $ fromList [("byr","1927"),("cid","154"),("ecl","grn"),("eyr","2027"),("hcl","#623a2f"),("hgt","177cm"),("iyr","2010"),("pid","725111435")]
-- True
--
-- >>> isValid $ fromList [("byr","1935"),("ecl","brn"),("eyr","2030"),("hcl","#888785"),("hgt","182cm"),("iyr","2013"),("pid","307171649")]
-- True
--
-- >>> isValid $ fromList [("byr","1938"),("ecl","amb"),("eyr","2028"),("hcl","#efcc98"),("hgt","178cm"),("iyr","2019"),("pid","568504071")]
-- True
--
-- >>> isValid $ fromList [("byr","1938"),("ecl","amb"),("eyr","2028"),("hcl","#efcc98"),("hgt","178cm"),("iyr","2019"),("cid","147")]
-- False
isValid ppt = all (flip member ppt) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValid' ppt = isValid ppt && all isJust ppt

count = (length .) . filter

between x y z = x <= y && y <= z

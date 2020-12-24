{-# LANGUAGE OverloadedStrings, GADTs, DeriveGeneric, DerivingVia, StandaloneDeriving, UndecidableInstances, TemplateHaskell, DeriveAnyClass, RankNTypes #-}
module Advent.Passport
  ( Passport(..)
  , UnvalidatedPassport(..)
  , ValidatedPassport(..)
  , UnvalidatedSetter(..)
  , Eye(..)
  , Height(..)
  , byr
  , iyr
  , eyr
  , hgt
  , ecl
  , hcl
  , pid
  , parsePassportField
  , passportParsers
  ) where

import Advent.Parsing
import Control.Lens
import Data.Char
import Data.Finite
import Data.Monoid.OneLiner (GMonoid(..))
import Data.Semigroup (Option(..))
import GHC.Generics
import Refined
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Barbies as B
import qualified Data.Text as T

type a <-> b = Refined (FromTo a b) Int
type n ** i  = Refined (SizeEqualTo n) [i]

data Eye    = AMB | BLU | BRN | GRY | GRN | HZL | OTH deriving (Show)
data Height = HCm (150 <-> 193) | HIn (59 <-> 76) deriving (Show)

data Passport f =
  Passport { _byr ::  f (1920 <-> 2002)
           , _iyr ::  f (2010 <-> 2020)
           , _eyr ::  f (2020 <-> 2030)
           , _hgt ::  f Height
           , _hcl ::  f (6 ** Finite 16)
           , _ecl ::  f Eye
           , _pid ::  f (9 ** Finite 10)
        -- , _cid :: Country Code
           } deriving (Generic, B.FunctorB, B.ApplicativeB, B.TraversableB, B.ConstraintsB)
makeLenses ''Passport

deriving instance B.AllBF Show f Passport => Show (Passport f)
deriving via GMonoid (Passport f) instance B.AllBF Semigroup f Passport => Semigroup (Passport f)
deriving via GMonoid (Passport f) instance B.AllBF Monoid f Passport => Monoid (Passport f)

type UnvalidatedPassport = Passport UnvalidatedField
type UnvalidatedField    = Const (Option T.Text)
type UnvalidatedSetter a = Setter' UnvalidatedPassport (UnvalidatedField a)
type ValidatedPassport   = Passport Identity

-- Passport Parsing
parseKey k = try (string k) <* char ':'
ident = T.pack <$> many (char '#' <|> alphaNumChar)

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
             parseField' :: T.Text -> UnvalidatedSetter a -> Parser UnvalidatedPassport
             parseField' k s = parseKey k *> ident >>= setValue s
             setValue s = return . flip (set s) mempty . Const . pure

passportParsers = Passport
  { _byr = number >>= refineFail . fromIntegral
  , _iyr = number >>= refineFail . fromIntegral
  , _eyr = number >>= refineFail . fromIntegral
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

parseHair = (char '#' *> takeWhileP Nothing isHexDigit) >>= refineFail . map (finite . fromIntegral . digitToInt) . T.unpack
parsePid = takeWhileP Nothing isDigit >>= refineFail .map (finite . fromIntegral . digitToInt) . T.unpack

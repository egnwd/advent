{-# LANGUAGE OverloadedStrings, GADTs, DeriveGeneric, DerivingVia, StandaloneDeriving, UndecidableInstances, TemplateHaskell, DeriveAnyClass, RankNTypes #-}
{-|
   Name: Passport Processing
   Url: <https://adventofcode.com/2020/day/4>
-}

module Main
  ( main
  ) where

import Advent

import Control.Lens
import Data.Finite
import Data.Foldable
import Data.Maybe
import Data.Monoid.OneLiner (GMonoid(..))
import Data.Semigroup (Option(..))
import GHC.Generics
import Prelude hiding (unlines)
import Refined
import Text.Megaparsec  hiding (count, between)
import Text.Megaparsec.Char hiding (count)

import qualified Barbies as B
import qualified Data.Text as T
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as L

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
type UnvalidatedField = Const (Option T.Text)
type UnvalidatedSetter a = Setter UnvalidatedPassport UnvalidatedPassport (UnvalidatedField a) (UnvalidatedField a)

main :: IO ()
main = do
  input <- getParsedDoubleLines 4 parseInput
  print $ validatePassports part1 input
  print $ validatePassports part2 input

-- | Parsing
parseKey k = try (string k) <* char ':'
ident = T.pack <$> many (char '#' <|> alphaNumChar)

parseField :: Parser UnvalidatedPassport
parseField = parseField' "byr" byr
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

parseInput :: Parser (Maybe (Passport (Const T.Text)))
parseInput = merge <$> parseField `sepEndBy` try (spaceChar >> notFollowedBy spaceChar)
  where merge = B.btraverse ((Const <$>) . getOption . getConst) . fold

validatePassports = (length .) . mapMaybe

part1 = id
part2 = id

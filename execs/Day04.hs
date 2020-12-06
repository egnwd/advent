{-|
   Name: Passport Processing
   Url: <https://adventofcode.com/2020/day/4>
-}

module Day04 (main) where

import Advent
import Advent.Passport

import Control.Lens
import Data.Foldable
import Data.Maybe
import Data.Semigroup (Option(..))
import Prelude hiding (unlines)
import Text.Megaparsec  hiding (count, between)

import qualified Barbies as B
import qualified Data.Text as T

main :: IO ()
main = do
  input <- getParsedDoubleLines 4 parsePassport
  print $ validatePassports part1 input
  print $ validatePassports part2 input

validatePassports = (length .) . mapMaybe

part1 :: Maybe (Passport (Const T.Text)) -> Maybe (Passport (Const T.Text))
part1 = id

part2 :: Maybe (Passport (Const T.Text)) -> Maybe ValidatedPassport
part2 x = (B.bzipWith (\p (Const t) -> Option $ parseMaybe p t) passportParsers) <$> x >>= B.btraverse (fmap Identity . getOption)

parsePassport = merge <$> parsePassportField `sepEndBy` singleSpace
  where merge = B.btraverse (fmap Const . getOption . getConst) . fold

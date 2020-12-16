{-|
   Name: Ticket Translation
   Url: <https://adventofcode.com/2020/day/16>
-}

module Day16 (main) where

import Advent
import Control.Arrow ((&&&))
import Control.Monad
import Data.Foldable
import Data.Ix (inRange)
import Data.List
import Data.Maybe
import Data.Monoid
import Prelude hiding (unlines)
import Text.Megaparsec (eof, sepBy, manyTill)
import Text.Megaparsec.Char (char, newline, printChar)
import qualified Data.Set as S
import qualified Data.Text as T

type Name = T.Text
data TicketRule = TR { ruleName :: Name, rulePredicate :: Int -> Bool }

instance Show TicketRule where
  show (TR name _) = T.unpack name

type Ticket = [Int]
type NamedTicket = [(Name, Int)]
data Notes = Notes
  { rules :: [TicketRule]
  , yourTicket :: Ticket
  , nearbyTickets :: [Ticket]
  } deriving Show

type Input  = Notes
type Output = Int

main :: IO ()
main = print . (part1 &&& part2) =<< getParsedInput 16 parseInput

-- | Parsing
parseInput :: Parser Notes
parseInput = do
  ticketRules <- parseTicketRules <* newline <* newline
  yourTicket <- parseYourTicket <* newline
  nearbyTickets <- parseNearbyTickets <* eof
  return $ Notes ticketRules yourTicket nearbyTickets

parseTicketRules :: Parser [TicketRule]
parseTicketRules = parseTicketRule `sepBy` singleSpace

parseTicketRule :: Parser TicketRule
parseTicketRule = do
  name <- T.pack <$> manyTill printChar (symbol ": ")
  (TR name .) . mkRule <$> parseRange <* symbol "or" <*> parseRange
    where
      parseRange = (,) <$> number <* char '-' <*> number
      mkRule a b n = a `inRange` n || b `inRange` n

parseYourTicket :: Parser Ticket
parseYourTicket = symbol "your ticket:" *> newline *> parseTicket <* newline

parseNearbyTickets :: Parser [Ticket]
parseNearbyTickets = symbol "nearby tickets:" *> newline *> (parseTicket `sepBy` newline)

parseTicket :: Parser Ticket
parseTicket = (fromIntegral <$> number) `sepBy` char ','

part1 :: Input -> Output
part1 = getSum . allInvalidFields

part2 :: Input -> Output
part2 notes = let myTicket = matchFields notes
               in getProduct . foldMap (Product . snd) $ filter (T.isPrefixOf "departure" . fst) myTicket

allInvalidFields :: Notes -> Sum Int
allInvalidFields notes = fold $ foldMap (map Sum . filter (not . validate notes)) (nearbyTickets notes)

validate :: Notes -> Int -> Bool
validate notes = getAny . foldMap ((Any .) . rulePredicate) (rules notes)

matchFields :: Notes -> NamedTicket
matchFields notes = namedTicket
  where
    namedTicket = zip (allocateFields (rules notes) groups) (yourTicket notes)
    groups = transpose . filter (all (validate notes)) $ nearbyTickets notes

allocateFields :: [TicketRule] -> [[Int]] -> [Name]
allocateFields rs gs = actualNames
  where
    actualNames         = map snd . sortOn fst . snd . mapAccumL pickName ruleNames $ possibleNames
    possibleNames       = sortOn (length . snd) . zip [0..] . map (namesForField rs) $ gs
    ruleNames           = S.fromList . map ruleName $ rs
    pickName rs (i, ns) = let rs' = S.delete n rs; n = head . filter (`S.member` rs) $ ns in (rs', (i, n))
    namesForField rs g  = mapMaybe (\r -> guard (all (rulePredicate r) g) *> Just (ruleName r)) rs

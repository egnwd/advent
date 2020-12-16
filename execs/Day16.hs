{-|
   Name: Ticket Translation
   Url: <https://adventofcode.com/2020/day/16>
-}

module Day16 (main) where

import Advent
import Prelude hiding (unlines)
import Control.Arrow ((&&&))
import Data.Foldable

import Data.Monoid

import qualified Data.Text as T
import Text.Megaparsec (eof, sepBy, manyTill)
import Text.Megaparsec.Char (char, newline, printChar)
import Data.Maybe
import Data.List
import qualified Data.Set as S


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
  range1 <- (,) <$> (fromIntegral <$> number) <* char '-' <*> (fromIntegral <$> number)
  symbol "or"
  range2 <- (,) <$> (fromIntegral <$> number) <* char '-' <*> (fromIntegral <$> number)
  return $ TR name (mkRule range1 range2)


mkRule :: (Int, Int) -> (Int, Int) -> Int -> Bool
mkRule (l1,u1) (l2,u2) n = between l1 u1 n || between l2 u2 n

between a b n = a <= n && n <= b

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
    validTickets = filter (all (validate notes)) (nearbyTickets notes)
    groups = transpose validTickets

allocateFields :: [TicketRule] -> [[Int]] -> [Name]
allocateFields rs gs = actualNames
  where
    ruleNames = S.fromList . map ruleName $ rs
    possibleNames = sortOn (length . snd) . zip [0..] . map (namesForField rs) $ gs
    actualNames = map snd . sortOn fst . snd . mapAccumL pickName ruleNames $ possibleNames

pickName rs (i, ns) = (rs', (i, n))
  where
    rs' = S.delete n rs
    n   = head . filter (`S.member` rs) $ ns

namesForField :: [TicketRule] -> [Int] -> [Name]
namesForField rs g = mapMaybe (acceptableRule g) rs

acceptableRule :: [Int] -> TicketRule -> Maybe Name
acceptableRule group ticketRule
  | all (rulePredicate ticketRule) group = pure $ ruleName ticketRule
  | otherwise = Nothing


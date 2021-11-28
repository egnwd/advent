-- |
-- Module      : AOC.Challenge.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.
module AOC.Challenge.Day13
  ( day13a,
    day13b,
  )
where

import AOC.Common (CharParser, pDecimal, pTok, pWord, parseLines)
import AOC.Solver ((:~>) (..))
import Control.Monad ((<=<))
import Data.List (permutations)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq ((:<|)), (<|))
import qualified Data.Sequence as S
import Text.Megaparsec ((<|>))
import Text.Megaparsec.Char (string)

type Seating = Map String (Map String Int)

parseHappinessRow :: CharParser (String, [(String, Int)])
parseHappinessRow = do
  a <- pWord
  pTok $ string "would"
  m <- (1 <$ pTok (string "gain")) <|> (-1 <$ pTok (string "lose"))
  h <- pTok pDecimal
  pTok $ string "happiness units by sitting next to"
  b <- init <$> pWord
  pure (a, [(b, m * h)])

buildSeating :: [(String, [(String, Int)])] -> Seating
buildSeating = M.map M.fromList . M.fromListWith (++)

getHappiness :: String -> String -> Seating -> Int
getHappiness a b = fromMaybe 0 . (M.lookup b <=< M.lookup a)

happiness :: Seating -> Seq String -> Int
happiness h (l :<| m :<| r :<| rs) = getHappiness m l h + getHappiness m r h + happiness h (m <| r <| rs)
happiness _ _ = 0

solve :: Seating -> Int
solve s = mx
  where
    people = M.keys s
    focus = head people
    possibilities = map (S.cycleTaking (length people + 2) . S.fromList) . filter ((== focus) . head) $ permutations people
    mx = maximum $ map (happiness s) possibilities

withMe :: Seating -> Seating
withMe s = s'
  where
    people = M.keys s
    me = "Elliot"
    s' = M.insert me (M.fromList $ zip people (repeat 0)) . M.map (M.insert me 0) $ s

day13a :: Seating :~> Int
day13a =
  MkSol
    { sParse = fmap buildSeating . parseLines parseHappinessRow,
      sShow = show,
      sSolve = Just . solve
    }

day13b :: Seating :~> Int
day13b =
  MkSol
    { sParse = fmap buildSeating . parseLines parseHappinessRow,
      sShow = show,
      sSolve = Just . solve . withMe
    }

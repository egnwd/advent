-- |
-- Module      : AOC.Challenge.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.

module AOC.Challenge.Day16 (
    day16a
  , day16b
  ) where

import AOC.Solver ((:~>)(..))
import AOC.Common
import Data.Maybe (listToMaybe)
import Data.Monoid (getAll, All(..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Map (Map)
import qualified Data.Map as M

data Sue = Sue
    { sueId :: Int
    , sueProps :: Map String Int
    } deriving Show

output :: Map String Int
output = M.fromList
    [ ("children", 3)
    , ("cats", 7)
    , ("samoyeds", 2)
    , ("pomeranians", 3)
    , ("akitas", 0)
    , ("vizslas", 0)
    , ("goldfish", 5)
    , ("trees", 3)
    , ("cars", 2)
    , ("perfumes", 1)
    ]

parseSue :: CharParser Sue
parseSue = do
    i <- pWord *> pDecimal <* pTok (char ':')
    ps <- parseProp `sepBy` pTok (char ',')
    pure $ Sue i (M.fromList ps)

parseProp :: CharParser (String, Int)
parseProp = (,) <$> (init <$> pWord) <*> pDecimal

checkProps :: (String -> Int -> Int -> Bool) -> Map String Int -> Bool
checkProps p = getAll . M.foldMapWithKey (\k a -> maybe mempty (All . p k a) $ M.lookup k output)

solve :: (String -> Int -> Int -> Bool) -> [Sue] -> Maybe Int
solve p = listToMaybe . map sueId . filter (checkProps p . sueProps)

recalibratedMFCSAM :: String -> Int -> Int -> Bool
recalibratedMFCSAM = \case
    "cats"        -> (>)
    "trees"       -> (>)
    "pomeranians" -> (<)
    "goldfish"    -> (<)
    _             -> (==)

day16a :: [Sue] :~> Int
day16a = MkSol
    { sParse = parseLines parseSue
    , sShow  = show
    , sSolve = solve (const (==))
    }

day16b :: [Sue] :~> Int
day16b = MkSol
    { sParse = parseLines parseSue
    , sShow  = show
    , sSolve = solve recalibratedMFCSAM
    }

-- |
-- Module      : AOC.Challenge.Day14
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 14.

module AOC.Challenge.Day14 (
    day14a
  , day14b
  ) where

import AOC.Solver
import AOC.Common
import Data.Maybe
import Text.Megaparsec.Char
import Control.Lens

type Distance = Int
type Score = Int
data RaceState = Race
    { isFlying :: Bool
    , _distance :: Distance
    , _score :: Score
    , time :: Int
    } deriving (Eq, Show)

$(makeLenses ''RaceState)

data Reindeer = Reindeer
    { rName :: String
    , rSpeed :: Int
    , rFlying :: Int
    , rResting :: Int
    } deriving (Eq, Show)

step :: (Reindeer, RaceState) -> (Reindeer, RaceState)
step (r, s) = (r, s')
    where
        s' = Race m' d' (_score s) (time s + 1)
        m' = time s `mod` (rFlying r + rResting r) < rFlying r
        d' = if m' then _distance s + rSpeed r else _distance s

awardPoints :: [(Reindeer, RaceState)] -> [(Reindeer, RaceState)]
awardPoints rs = over (mapped . _2) update rs
    where
        mx = fromMaybe 0 $ bestReindeerBy distance rs
        update s
          | s ^. distance == mx = s & score %~ succ
          | otherwise = s


emptyState :: RaceState
emptyState = Race True 0 0 0

bestReindeerBy :: (Ord s, Field2 r r a b) => Lens a b s s -> [r] -> Maybe s
bestReindeerBy f = maximumOf (traverse . _2 . f)

-- Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
parseReindeer :: CharParser Reindeer
parseReindeer =
    Reindeer
        <$> pWord <* pTok (string "can fly")
        <*> pTok pDecimal <* pTok (string "km/s for")
        <*> pTok pDecimal <* pTok (string "seconds, but then must rest for")
        <*> pTok pDecimal <* pTok (string "seconds.")

day14a :: [Reindeer] :~> Distance
day14a = MkSol
    { sParse = parseLinesOrError parseReindeer
    , sShow  = show
    , sSolve = bestReindeerBy distance . (!! dyno_ "seconds" 2503) . iterate (map step) . flip zip (repeat emptyState)
    }

day14b :: [Reindeer] :~> Score
day14b = MkSol
    { sParse = parseLinesOrError parseReindeer
    , sShow  = show
    , sSolve = bestReindeerBy score . (!! dyno_ "seconds" 2503) . iterate (awardPoints . map step) . flip zip (repeat emptyState)
    }

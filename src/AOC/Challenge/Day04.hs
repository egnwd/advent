{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day04 (
    day04a
  , day04b
  ) where

import           AOC.Solver ((:~>)(..))
import           AOC.Common (parseLines, pTok, pDecimal, CharParser)
import           Data.IntMap (IntMap)
import           Data.IntSet (IntSet)

import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Text.Megaparsec                as P

type Card = IntSet
type Scratchcard = IntSet

parseCard :: CharParser (Int, (Card, Scratchcard))
parseCard = (,) <$> cId <*> ((,) <$> cNums <*> mine)
    where
        cId = pTok "Card" *> pDecimal <* pTok ":"
        cNums = IS.fromList <$> P.many (pTok pDecimal)
        mine = IS.fromList <$> (pTok "|" *> P.many (pTok pDecimal))

getPoints :: Int -> Int
getPoints 0 = 0
getPoints n = round $ 2 ^^ (n-1)

getMatches :: (Card, Scratchcard) -> Int
getMatches = IS.size . uncurry IS.intersection

run :: IntMap (Card, Scratchcard) -> IntMap Int
run cs0 = IM.foldlWithKey go startingCopies cs0
    where
        startingCopies = IM.map (const 1) cs0
        go cs cId cnm = let p = getMatches cnm
                         in foldr (IM.adjust (+ (cs IM.! cId))) cs [cId+1..cId+p]

day04a :: [(Int, (Card, Scratchcard))] :~> _
day04a = MkSol
    { sParse = parseLines parseCard
    , sShow  = show
    , sSolve = Just . sum . map (getPoints . getMatches . snd)
    }

day04b :: IntMap (Card, Scratchcard) :~> _
day04b = MkSol
    { sParse = fmap IM.fromList . parseLines parseCard
    , sShow  = show
    , sSolve = Just . sum . run
    }

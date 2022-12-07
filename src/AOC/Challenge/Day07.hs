{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day07 (
    day07a
  , day07b
  ) where

import           AOC.Solver ((:~>)(..))
import           AOC.Common (parseMaybeLenient, pTok, pDecimal, pWord, CharParser)
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Data.Functor.Foldable          as F
import           Data.Functor.Foldable.TH (makeBaseFunctor)

data Entry = File Int | Dir [Entry] deriving (Eq, Ord, Show)

$(makeBaseFunctor ''Entry)

buildSizes :: EntryF (Int, [Int]) -> (Int, [Int])
buildSizes (FileF sz) = (sz, [])
buildSizes (DirF sz) = let (totals, concat->subs) = unzip sz
                           total = sum totals
                        in (total, total : subs)

totalSize :: Entry -> Int
totalSize = sum . filter (<= 100000) . snd . F.cata buildSizes

totalSpace, spaceNeeded :: Int
totalSpace = 70000000
spaceNeeded = 30000000

deleteSmallest :: Entry -> Int
deleteSmallest e = let (total, sizes) = F.cata buildSizes e
                       needToFree = spaceNeeded - (totalSpace - total)
                    in minimum . filter (>= needToFree) $ sizes

parseStructure :: CharParser (Entry)
parseStructure = do
    P.try (P.notFollowedBy ("$ cd .." <* P.newline))
    pTok "$ cd" *> pWord <* P.newline
    "$ ls" <* P.newline
    files <- parseFiles
    dirs <- P.many parseStructure
    P.optional ("$ cd .." <* P.newline)
    return $ Dir (files ++ dirs)
    where
        parseFiles = P.sepEndBy (P.try (P.skipManyTill parseDir parseFile)) P.newline <* P.skipMany parseDir
        parseDir = pTok "dir" *> pWord <* P.newline
        parseFile = File <$> (pTok pDecimal) <* pWord

day07a :: Entry :~> Int
day07a = MkSol
    { sParse = parseMaybeLenient parseStructure
    , sShow  = show
    , sSolve = Just . totalSize
    }

day07b :: Entry :~> Int
day07b = MkSol
    { sParse = parseMaybeLenient parseStructure
    , sShow  = show
    , sSolve = Just . deleteSmallest
    }

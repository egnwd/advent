-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.

module AOC.Challenge.Day04 (
    day04a
  , day04b
  ) where

import           AOC.Solver                ((:~>)(..), dyno_)
import           AOC.Common                (CharParser, pDecimal, parseLines, revFreq, pTok)
import           Data.Monoid               (Sum(..), getSum)
import           Data.List                 (find, intercalate)
import           Data.Maybe                (mapMaybe)
import           Data.Char                 (ord, chr, isLower)
import           Data.Functor              (($>))
import           Control.Monad             (guard)
import           Text.Megaparsec           (many, satisfy, between, endBy)
import           Text.Megaparsec.Char      (char)
import qualified Data.IntMap        as M
import qualified Data.Set.NonEmpty  as NES
import qualified Data.List.NonEmpty as NE

type Name = String
type SectionId = Int
type CheckSum = String


parser :: CharParser (Name, SectionId, CheckSum)
parser = do
    name <- intercalate "-" <$> (pName `endBy` char '-')
    sectionId <- pDecimal
    checkSum <- between (char '[') (char ']') pName
    pure (name, sectionId, checkSum)
        where
            pName = pTok $ many (satisfy isLower)

validCheckSum :: Name -> CheckSum -> Bool
validCheckSum name checkSum = hash == checkSum
    where
        hash = take 5 . filter (/='-') . concatMap (NE.toList . NES.toAscList . snd) . M.toDescList . revFreq $ name

solve :: (Name, SectionId, CheckSum) -> Maybe (Sum Int)
solve (name, sid, checkSum) = guard (validCheckSum name checkSum) $> Sum sid

getNames :: (Name, SectionId, CheckSum) -> Maybe (SectionId, Name)
getNames (name, sid, checkSum) = guard (validCheckSum name checkSum) $> (sid, caesarCipher sid name)

caesarCipher :: Int -> Name -> Name
caesarCipher rot = map apply
    where
        start = ord 'a'
        apply '-' = ' '
        apply (ord->c) = chr $ (c-start+rot) `mod` 26 + start

day04a :: [(Name, SectionId, CheckSum)] :~> Int
day04a = MkSol
    { sParse = parseLines parser
    , sShow  = show
    , sSolve = fmap getSum . foldMap solve
    }

day04b :: [(Name, SectionId, CheckSum)] :~> SectionId
day04b = MkSol
    { sParse = parseLines parser
    , sShow  = show
    , sSolve = fmap fst . find ((== dyno_ "room" "northpole object storage") . snd) . mapMaybe getNames
    }

{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.

module AOC.Challenge.Day13 (
    day13a
  , day13b
  ) where

import           AOC.Solver           ((:~>)(..))
import           AOC.Common           (parseMaybeLenient, pTok, pDecimal, Point, CharParser)
import           Advent.OCR           (parseLettersWith)
import           Control.Lens         (view, _head, (^?))
import           Control.Monad        (void)
import           Data.Bifunctor       (first)
import           Linear               (V2(..), _x, _y)
import           Text.Megaparsec      (many, try, eof, (<|>))
import           Text.Megaparsec.Char (char, newline)
import qualified Data.Set as S

type TransparentPaper = S.Set Point
data Fold = FX Int | FY Int

parser :: CharParser (TransparentPaper, [Fold])
parser =  (,) <$> parsePoints <*> parseFolds

parsePoints :: CharParser TransparentPaper
parsePoints = S.fromList <$> many (try (parsePoint <* newline)) <* newline
    where
        parsePoint = V2 <$> pDecimal <* char ',' <*> pDecimal

parseFolds :: CharParser [Fold]
parseFolds = many $ foldAlong *> (parseX <|> parseY) <* trailing
    where
        foldAlong = pTok "fold along"
        parseX = FX <$> ("x=" *> pDecimal)
        parseY = FY <$> ("y=" *> pDecimal)
        trailing = void newline <|> eof

fullyFold :: TransparentPaper -> [Fold] -> TransparentPaper
fullyFold = foldl applyFold

applyFold :: TransparentPaper -> Fold -> TransparentPaper
applyFold points f = uncurry S.union . first remap . partition $ points
    where
        remap = S.map (`remapPoint` f)
        partition = S.partition (`beyondFold` f)

remapPoint :: Point -> Fold -> Point
remapPoint (V2 x y) = \case
    FX fx -> V2 (2*fx-x) y
    FY fy -> V2 x        (2*fy-y)

beyondFold :: Point -> Fold -> Bool
beyondFold (V2 x y) = \case
    FX fx -> x > fx
    FY fy -> y > fy

day13a :: (TransparentPaper, [Fold]) :~> Int
day13a = MkSol
    { sParse = parseMaybeLenient parser
    , sShow  = show
    , sSolve = \(ps, fs) -> S.size . applyFold ps <$> fs ^? _head
    }

day13b :: (TransparentPaper, [Fold]) :~> String
day13b = MkSol
    { sParse = parseMaybeLenient parser
    , sShow  = id
    , sSolve = parseLettersWith (view _x) (view _y) . uncurry fullyFold
    }

{-# LANGUAGE OverloadedStrings #-}

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

import AOC.Solver ((:~>)(..), dyno_)
import AOC.Common (parseMaybeLenient, (!?), (-?), freqs, foldMapKeysWith, CharParser)
import Control.Lens (maximumOf, minimumOf)
import Text.Megaparsec (many, (<|>), takeWhile1P, anySingle, eof)
import Control.Monad ((<=<), void)
import Text.Megaparsec.Char (newline)
import qualified Data.Map as M

type Element = Char
type FormulaMapping = M.Map (Element, Element) Element
type FormulaFrequencies a = M.Map a Int

parser :: CharParser (String, FormulaMapping)
parser = (,) <$> (formulaP <* newline <* newline) <*> (M.fromList <$> many (mapping <* trailing))
    where
       formulaP = takeWhile1P Nothing (/= '\n')
       lhs = (,) <$> anySingle <*> anySingle
       rhs = anySingle
       mapping = (,) <$> (lhs <* " -> ") <*> rhs
       trailing = void newline <|> eof

generateFormula :: Int -> String -> FormulaMapping -> Maybe (FormulaFrequencies (Element, Element))
generateFormula n formula m = (!? n) . iterate substitute $ formula'
    where
        formula' = freqs . pairwise $ formula
        pairwise ls = zip ls (tail ls)
        substitute = foldMapKeysWith (+) new
        new k@(a,b) = case M.lookup k m of { Just c -> [(a,c), (c,b)]; Nothing -> [k] }

formulaFrequencies :: FormulaFrequencies (Element, Element) -> FormulaFrequencies Element
formulaFrequencies = M.mapKeysWith (+) snd

scoreFormula :: (Num n, Ord n) => M.Map a n -> Maybe n
scoreFormula f = mx -? mn
    where
        mx = maximumOf traverse f
        mn = minimumOf traverse f

solve :: Int -> (String, FormulaMapping) -> Maybe Int
solve n = scoreFormula . formulaFrequencies <=< uncurry (generateFormula n)

day14 :: Int -> (String, FormulaMapping) :~> Int
day14 n = MkSol
    { sParse = parseMaybeLenient parser
    , sShow  = show
    , sSolve = solve (dyno_ "steps" n)
    }


day14a :: (String, FormulaMapping) :~> Int
day14a = day14 10

day14b :: (String, FormulaMapping) :~> Int
day14b = day14 40

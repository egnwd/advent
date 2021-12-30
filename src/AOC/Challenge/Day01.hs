{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.

module AOC.Challenge.Day01 (
    day01a
  , day01b
  ) where

import           AOC.Solver      ((:~>)(..))
import           AOC.Common      (CharParser, pDecimal, parseMaybeLenient, dirVec, Point, Dir(..), lineTo)
import           Data.List       (find)
import           Linear          (V2(..), (^*), zero)
import           Text.Megaparsec (many, (<|>), optional)
import           Text.Megaparsec.Char
import qualified Data.Set as S

parser :: CharParser [(Dir, Int)]
parser = many (parseInstr <* optional ", ")
    where
        parseDir = (East <$ char 'R') <|> (West <$ char 'L')
        parseInstr = (,) <$> parseDir <*> pDecimal

solve :: [(Dir, Int)] -> Point
solve = snd . foldl (\(h,p) (d,l) -> (h<>d, p + dirVec (h<>d) ^* l)) (North, zero)

solveb :: [(Dir, Int)] -> Maybe Point
solveb = solve' S.empty (North, zero)
    where
        solve' _    _      [] = Nothing
        solve' seen (h, p) ((d,l):rest)
          = case find (`S.member` seen) ps of
              Nothing -> solve' seen' (h',p') rest
              Just x -> Just x
            where
                ps = init $ lineTo (V2 p p')
                seen' = foldr S.insert seen ps
                h' = h<>d
                p' = p + dirVec h' ^* l

day01a :: [(Dir, Int)] :~> Int
day01a = MkSol
    { sParse = parseMaybeLenient parser
    , sShow  = show
    , sSolve = Just . sum . abs . solve
    }

day01b :: [(Dir, Int)] :~> Int
day01b = MkSol
    { sParse = parseMaybeLenient parser
    , sShow  = show
    , sSolve = fmap (sum . abs) . solveb
    }

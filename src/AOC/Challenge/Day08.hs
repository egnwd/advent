{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day08 (
    day08a
  , day08b
  ) where

import           AOC.Prelude
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M

splitOn :: T.Text -> String -> [String]
splitOn c = map T.unpack . T.splitOn c . T.pack

parser :: String -> [[[String]]]
parser = map (map words . splitOn "|") . lines

outputs, signals :: [[String]] -> [String]
outputs [_, o] = o
signals [s, _] = s

solvea :: [[[String]]] -> Int
solvea = countTrue ((`elem` [2,3,4,7]) . length) . concatMap outputs

zero, one, two, three, four, five, six, seven, eight, nine :: S.Set Char
zero  = S.fromList "abcefg"
one   = S.fromList "cf"
two   = S.fromList "acdeg"
three = S.fromList "acdfg"
four  = S.fromList "bcdf"
five  = S.fromList "abdfg"
six   = S.fromList "abdefg"
seven = S.fromList "acf"
eight = S.fromList "abcdefg"
nine  = S.fromList "abcdfg"

digits :: M.Map (S.Set Char) Int
digits = M.fromList $ zip [zero, one, two, three, four, five, six, seven, eight, nine] [0..]

solveb :: [[[String]]] -> Maybe Int
solveb ds = sum <$> zipWithM (\r -> fmap (\m -> numberFromDigits . map (pickNumber . translate m) . outputs $ r)) ds answers
    where
        uniqueChoices = map (pickUnique . M.toList . M.fromListWith S.intersection . choices . signals) ds
        answers = zipWith (\r -> find (\m -> all (\s -> translate m s `M.member` digits) . signals $ r)) ds uniqueChoices
        translate m = S.fromList . map (m M.!)

pickNumber :: S.Set Char -> Int
pickNumber = (digits M.!)

numberFromDigits :: [Int] -> Int
numberFromDigits = foldl (\n d -> n * 10 + d) 0

choices :: [String] -> [(Char, S.Set Char)]
choices = concatMap choices'
    where
        choices' s | length s == 2 = map (, one) s
                   | length s == 3 = map (, seven) s
                   | length s == 4 = map (, four) s
                   | length s == 7 = map (, eight) s
                   | otherwise = []

day08a :: _ :~> _
day08a = MkSol
    { sParse = Just . parser
    , sShow  = show
    , sSolve = Just . solvea
    }

day08b :: _ :~> _
day08b = MkSol
    { sParse = Just . parser
    , sShow  = show
    , sSolve = solveb
    }

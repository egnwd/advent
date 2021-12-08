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

import AOC.Prelude
import Control.Lens
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.Loops

type Segments = S.Set Char

data Entry = Entry { signals :: [Segments], outputs :: [Segments] } deriving (Eq, Show)

parser :: String -> [Entry]
parser = map (uncurry Entry . over both (map S.fromList) . second (drop 1) . break (=="|") . words) . lines

zero, one, two, three, four, five, six, seven, eight, nine :: Segments
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

digits :: M.Map Segments Int
digits = M.fromList $ zip [zero, one, two, three, four, five, six, seven, eight, nine] [0..]

solveb :: [Entry] -> Maybe Int
solveb ds = sum <$> zipWithM (\e t -> t >>= outputToNumber (outputs e)) ds translations
    where
        outputToNumber o t = numberFromDigits <$> mapM (pickNumber <=< translate t) o
        translations       = map (decodeEntry . signals) ds
        decodeEntry e      = join $ findMOf traverse (\m -> allM (fmap (`M.member` digits) . translate m) e) (choices e)
        translate t        = fmap S.fromList . mapM (`M.lookup` t) . S.toList

pickNumber :: Segments -> Maybe Int
pickNumber = flip M.lookup digits

numberFromDigits :: (Foldable t) => t Int -> Int
numberFromDigits = foldl (\n d -> n * 10 + d) 0

choices :: [Segments] -> [M.Map Char Char]
choices = pickUnique . M.toList . M.fromListWith S.intersection . S.toList . S.unions . map choices'
    where
        choices' s | S.size s == 2 = S.map (, one)   s
                   | S.size s == 3 = S.map (, seven) s
                   | S.size s == 4 = S.map (, four)  s
                   | S.size s == 7 = S.map (, eight) s
                   | otherwise = S.empty

day08a :: [Entry] :~> Int
day08a = MkSol
    { sParse = Just . parser
    , sShow  = show
    , sSolve = Just . countTrue ((`elem` [2,3,4,7]) . length) . concatMap outputs
    }

day08b :: [Entry] :~> Int
day08b = MkSol
    { sParse = Just . parser
    , sShow  = show
    , sSolve = solveb
    }

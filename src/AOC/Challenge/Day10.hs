{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day10 (
    day10a
  , day10b
  ) where

import           AOC.Prelude

data AutocompleteScore a = (Num a) => Auto { autoScore :: a }

instance Semigroup (AutocompleteScore a) where
    (Auto s) <> (Auto s') = Auto ((s * 5) + s')

instance (Num a) => Monoid (AutocompleteScore a) where
    mempty = Auto 0
    mconcat = foldl (<>) mempty

isOpen :: Char -> Bool
isOpen = (`elem` "({[<")

matching :: String -> Bool
matching = \case
  "()" -> True
  "[]" -> True
  "{}" -> True
  "<>" -> True
  _    -> False

scorea :: Char -> Sum Integer
scorea = \case
    ')' -> Sum 3
    ']' -> Sum 57
    '}' -> Sum 1197
    '>' -> Sum 25137
    _   -> mempty

scoreb :: Char -> AutocompleteScore Integer
scoreb = \case
    '(' -> Auto 1
    '[' -> Auto 2
    '{' -> Auto 3
    '<' -> Auto 4
    _   -> mempty


solvea :: String -> Maybe (Sum Integer)
solvea [] = Nothing
solvea (b:bs) = solve' bs [b]
    where
        solve' [] _ = Nothing
        solve' (_:_) [] = Nothing
        solve' (b':bs') (o:os) | isOpen b'        = solve' bs' (b':o:os)
                               | matching [o, b'] = solve' bs' os
                               | otherwise        = Just $ scorea b'

solveb :: String -> Maybe Integer
solveb [] = Nothing
solveb (b:bs) = solve' bs [b]
    where
        solve' _ [] = Nothing
        solve' [] os = Just . autoScore . mconcat . map scoreb $ os
        solve' (b':bs') (o:os) | isOpen b'        = solve' bs' (b':o:os)
                               | matching [o, b'] = solve' bs' os
                               | otherwise        = Nothing

middle :: (Ord a) => [a] -> Maybe a
middle ns = sort ns !? (length ns `div` 2)

day10a :: [String] :~> Integer
day10a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = fmap getSum . foldMap solvea
    }

day10b :: [String] :~> Integer
day10b = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = middle . mapMaybe solveb
    }

{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day08 (
    day08a
  , day08b
  ) where

import           AOC.Solver ((:~>)(..))
import           AOC.Common (CharParser, loopEither, listTup, parseLines)
import           Control.Monad ((<=<))
import           Data.List (isSuffixOf)
import           Data.List.Split (splitOn)
import           Data.Foldable (foldr')
import           Control.Arrow (second)
import           Data.Map                       (Map, (!))
import qualified Data.Map                       as M
import qualified Data.Set                       as S
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import Control.Lens (_1, _2, (^.))

mapParser :: CharParser (String, (String, String))
mapParser = (,) <$> (ident <* " = ") <*> ((,) <$> ("(" *> ident <* ", ") <*> (ident <* ")"))
    where ident = P.many P.alphaNumChar

run :: String -> Map String (String, String) -> Int
run is0 mp = loopEither (step (mp !) (== "ZZZ")) ("AAA", 1, cycle is0)

run2 :: String -> Map String (String, String) -> Int
run2 is0 mp = foldr' lcm 1 . map (\a -> loopEither (step (mp !) (`S.member` zs)) (a, 1, cycle is0)) $ as
    where
        as = M.keys . M.filterWithKey (const . ("A" `isSuffixOf`)) $ mp
        zs = M.keysSet . M.filterWithKey (const . ("Z" `isSuffixOf`)) $ mp

step :: (String -> (String, String)) -> (String -> Bool) -> (String, Int, String) -> Either Int (String, Int, String)
step _ _ (_, _, []) = error "Should be an inifite list"
step next stop (l, n, (i:is)) = let side = if i == 'L' then _1 else _2
                                    l' = next l ^. side
                                 in if stop l'
                                        then Left n
                                        else Right (l', n+1, is)

parse :: String -> Maybe (String, Map String (String, String))
parse = sequence . second (fmap M.fromList . parseLines mapParser) <=< listTup . splitOn "\n\n"

day08a :: (String, Map String (String, String)) :~> Int
day08a = MkSol
    { sParse = parse
    , sShow  = show
    , sSolve = Just  . uncurry run
    }

day08b :: (String, Map String (String, String)) :~> Int
day08b = MkSol
    { sParse = parse
    , sShow  = show
    , sSolve = Just . uncurry run2
    }

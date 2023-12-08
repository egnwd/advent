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

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP
import Control.Lens

mapParser :: CharParser (String, (String, String))
mapParser = do
    label <- (P.many P.alphaNumChar) <* " = "
    opts <- (,) <$> ("(" *> (P.many P.alphaNumChar) <* ", ") <*> ((P.many P.alphaNumChar) <* ")")
    return (label, opts)

run is0 mp = loopEither go ("AAA", 1, cycle is0)
    where
        go (l, n, (i:is)) = let side = if i == 'L' then _1 else _2
                             in case (mp M.! l) ^. side of
                                  "ZZZ" -> Left n
                                  l' -> Right (l', n+1, is)

run2 is0 mp = foldr' lcm 1 . map (\a -> loopEither go (a, 1, cycle is0)) $ as
    where
        as = M.keys . M.filterWithKey (\k _ -> "A" `isSuffixOf` k) $ mp
        zs = M.keysSet . M.filterWithKey (\k _ -> "Z" `isSuffixOf` k) $ mp
        go (l, n, (i:is)) = let side = if i == 'L' then _1 else _2
                                l' = (mp M.! l) ^. side
                             in if l' `S.member` zs
                                  then Left n
                                  else Right (l', n+1, is)

day08a :: _ :~> _
day08a = MkSol
    { sParse = sequence . second (fmap M.fromList . parseLines mapParser) <=< listTup . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . uncurry run
    }

day08b :: _ :~> _
day08b = MkSol
    { sParse = sParse day08a
    , sShow  = show
    , sSolve = Just . uncurry run2
    }

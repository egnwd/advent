{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : AOC.Challenge.Day14
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 14.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day14 (
    day14a
  , day14b
  ) where

import           AOC.Prelude

import Linear
import Control.Lens

import Debug.Trace

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

parsePath :: CharParser (S.Set Point)
parsePath = do
    (p:ps) <- parsePoint `P.sepBy` (pTok "->")
    return (S.fromList . concat . snd . mapAccumL (\a b -> (b, lineTo (V2 a b))) p $ ps)

parsePoint :: CharParser Point
parsePoint = V2 <$> pNum <* "," <*> pNum
    where
        pNum = pTok pDecimal

countSand :: S.Set Point -> _
countSand = subtract 1 . fst . indexedFixedPoint dropGrain
    where
        dropGrain rocks = let restingPlace = loopEither (moveGrain rocks) (V2 500 0)
                           in case restingPlace of
                                Just s -> S.insert s rocks
                                Nothing -> rocks
        moveGrain :: S.Set Point -> Point -> Either (Maybe Point) Point
        moveGrain rocks s = let down = s + V2 0 1
                                left = s + V2 (-1) 1
                                right = s + V2 1 1
                                Just limit = maximumOf (folded . _y) rocks
                             in if (s ^. _y) >= limit
                                   then Left Nothing
                                   else if down `S.notMember` rocks
                                       then Right down
                                       else if left `S.notMember` rocks
                                            then Right left
                                            else if right `S.notMember` rocks
                                                then Right right
                                                else Left (Just s)

findSafeZone :: S.Set Point -> _
findSafeZone rocks = fst . indexedFixedPoint dropGrain $ rocks
    where
        Just limit = maximumOf (folded . _y) rocks
        dropGrain items = let restingPlace = loopEither (moveGrain items) (V2 500 0)
                           in case restingPlace of
                                Just s -> S.insert s items
                                Nothing -> items
        moveGrain :: S.Set Point -> Point -> Either (Maybe Point) Point
        moveGrain items s
          = let down = s + V2 0 1
                left = s + V2 (-1) 1
                right = s + V2 1 1
             in if (s ^. _y) == limit + 1
                   then Left (Just s)
                   else if down `S.notMember` items
                       then Right down
                       else if left `S.notMember` items
                            then Right left
                            else if right `S.notMember` items
                                then Right right
                                else if s == (V2 500 0) then Left Nothing else Left (Just s)

day14a :: _ :~> _
day14a = MkSol
    { sParse = fmap S.unions . parseLines parsePath
    , sShow  = show
    , sSolve = Just . countSand
    }

day14b :: _ :~> _
day14b = MkSol
    { sParse = fmap S.unions . parseLines parsePath
    , sShow  = show
    , sSolve = Just . findSafeZone
    }

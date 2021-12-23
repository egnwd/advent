{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day22
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 22.

module AOC.Challenge.Day22 (
    day22a
  , day22b
  ) where

import AOC.Solver          ((:~>)(..))
import AOC.Common          (CharParser, parseLines, pTok, pDecimal)
import Control.Applicative ((<|>))
import Control.Lens        (Lens', LensLike', Const, (^.), (^..), (&), (.~), beside, to)
import Control.Monad       (guard)
import Data.Bifunctor      (second)
import Data.Distributive   (Distributive)
import Data.Foldable       (foldl')
import Data.Functor        (($>))
import Data.Maybe          (mapMaybe, isNothing)
import Data.Monoid         (Endo)
import Linear              (V2(..), V3(..), _x, _y, _z, transpose)
import Numeric.Lens        (adding)
import Text.Megaparsec     (optional)

data Instruction = On | Off deriving Eq
type Cube = V3 Interval
type Interval = V2 Int

-- ^ Parsing

parserStep :: CharParser (Instruction, Cube)
parserStep = (,) <$> parseInstruction <*> parseCube
    where
        parseInstruction = (On <$ pTok "on") <|> (Off <$ pTok "off")
        parseCube = V3 <$> ("x=" *> parseInterval) <*> ("y=" *> parseInterval) <*> ("z=" *> parseInterval)
        parseInterval = V2 <$> (pDecimal <* "..") <*> (pDecimal <* optional ",")

-- ^ Cube Lenses

_xmnInc, _xmnExc, _xmxInc, _xmxExc :: Lens' Cube Int
_ymnInc, _ymnExc, _ymxInc, _ymxExc :: Lens' Cube Int
_zmnInc, _zmnExc, _zmxInc, _zmxExc :: Lens' Cube Int
_xmnInc = _x . _mn
_ymnInc = _y . _mn
_zmnInc = _z . _mn
_xmnExc = _xmnInc . adding 1
_ymnExc = _ymnInc . adding 1
_zmnExc = _zmnInc . adding 1

_xmxInc = _x . _mx
_ymxInc = _y . _mx
_zmxInc = _z . _mx
_xmxExc = _xmxInc . adding 1
_ymxExc = _ymxInc . adding 1
_zmxExc = _zmxInc . adding 1

_mn :: Lens' Interval Int
_mn = _x
_mx :: Lens' Interval Int
_mx = _y

-- ^ Cube intersection/subset/disjoint operations

intersectionV :: V2 Int -> V2 Int -> Maybe (V2 Int)
intersectionV (V2 mn1 mx1) (V2 mn2 mx2)
  = let mxmn = max mn1 mn2
        mnmx = min mx1 mx2
     in guard (mxmn <= mnmx) $> V2 mxmn mnmx

intersectionCube :: (Distributive g, Traversable g) => g (V2 Int) -> g (V2 Int) -> Maybe (g (V2 Int))
intersectionCube c1 c2 = mapM (\(V2 i1 i2) -> intersectionV i1 i2) . transpose $ V2 c1 c2

subset :: (Eq (g (V2 Int)), Distributive g, Traversable g) => g (V2 Int) -> g (V2 Int) -> Bool
subset a b = intersectionCube a b == Just a

disjoint :: Cube -> Cube -> Bool
disjoint c1 c2 = isNothing (intersectionCube c1 c2)

half :: Lens' Cube Interval -> Int -> Cube -> (Cube, Cube)
half dir about cube | cube ^. dir . _mn < about && about <= cube ^. dir . _mx = (cube & dir . _mx .~ (about - 1), cube & dir . _mn .~ about)
half _ about cube = error $ "half " ++ show (about, cube)

-- ^ Solution Helpers

merge :: Instruction -> Cube -> [Cube] -> [Cube]
merge isOn c0 = go
    where
        recurseLeft, recurseRight :: [Cube] -> LensLike' (Const (Endo [Cube])) (Cube, Cube) Cube
        recurseLeft cs = beside (to (go . (:cs)).traverse) id
        recurseRight cs = beside id (to (go . (:cs)).traverse)

        go :: [Cube] -> [Cube]
        go [] = [c0 | isOn==On]
        go (c:cs)
          -- On command when everything in the command is already on
          | isOn == On && c0 `subset` c = c : cs
          -- Command has no influence of this block
          | c `disjoint` c0 = c : go cs
          -- Block is a subset of the command, ignore
          | c `subset` c0 = go cs
          -- All the cases when the blocks are overlapping partially
          -- Minimum corner is smaller than the minimum corner of the command (recurse on the "right" inclusive)
          | c ^. _xmnInc < c0 ^. _xmnInc = half _x (c0 ^. _xmnInc) c ^.. recurseRight cs
          | c ^. _ymnInc < c0 ^. _ymnInc = half _y (c0 ^. _ymnInc) c ^.. recurseRight cs
          | c ^. _zmnInc < c0 ^. _zmnInc = half _z (c0 ^. _zmnInc) c ^.. recurseRight cs
          -- Maximum corner is larger than the maximum corner of the command (recurse on the "left" exclusive)
          | c ^. _xmxExc > c0 ^. _xmxExc = half _x (c0 ^. _xmxExc) c ^.. recurseLeft cs
          | c ^. _ymxExc > c0 ^. _ymxExc = half _y (c0 ^. _ymxExc) c ^.. recurseLeft cs
          | c ^. _zmxExc > c0 ^. _zmxExc = half _z (c0 ^. _zmxExc) c ^.. recurseLeft cs
          -- Something has gone wrong
          | otherwise = error "Uh oh"

solve :: [(Instruction, Cube)] -> [Cube]
solve = foldl' (\cs (isOn, c) -> merge isOn c cs) []

-- ^ "Scoring"

volume :: Cube -> Int
volume = product . fmap (\(V2 mn mx) -> mx-mn+1)

countOnReactors :: [Cube] -> Int
countOnReactors = sum . map volume

-- ^ Solutions

day22a :: [(Instruction, Cube)] :~> Int
day22a = MkSol
    { sParse = parseLines parserStep
    , sShow  = show
    , sSolve = Just . countOnReactors . solve . justValidRange
    }
        where justValidRange = mapMaybe (sequence . second (intersectionCube (return $ V2 (-50) 50)))

day22b :: _ :~> _
day22b = MkSol
    { sParse = parseLines parserStep
    , sShow  = show
    , sSolve = Just . countOnReactors . solve
    }


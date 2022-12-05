{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day05 (
    day05a
  , day05b
  ) where

import           AOC.Solver ((:~>)(..))
import           AOC.Common (CharParser, parseMaybeLenient, pDecimal, pTok)
import           Data.Foldable (foldl')
import           Control.Applicative ((<|>))
import           Data.Maybe (catMaybes)
import           Data.List (transpose)
import           Control.Monad (void)
import           Control.Monad.Combinators (optional, some, sepEndBy1, skipManyTill)
import           Control.Lens (preview, _head)
import qualified Data.IntMap                    as IM
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P

type Crate = Char
type Ship = IM.IntMap [Crate]
data Instruction = Move !Int !Int !Int
type CraneArm = Ship -> Int -> Int -> [Crate]

parsePlanningSheet :: CharParser (Ship, [Instruction])
parsePlanningSheet = do
    s <- IM.fromList . zip [1..] . map catMaybes . transpose <$> parseCrates
    pNumbers <* P.newline
    pl <- parsePlan
    return (s, pl)

pNumbers :: CharParser ()
pNumbers = void $ skipManyTill P.anySingle P.newline

parseCrates :: CharParser [[Maybe Crate]]
parseCrates = some (pCrate <* optional (P.char ' ')) `sepEndBy1` P.newline
    where
        pCrate = Just <$> ("[" *> P.upperChar <* "]") <|> Nothing <$ "   "

parsePlan :: CharParser [Instruction]
parsePlan = pPlan `sepEndBy1` P.newline
    where
        pPlan = Move <$> (labelledN "move") <*> (labelledN "from") <*> (labelledN "to")
        labelledN l = pTok l *> pTok pDecimal

moveCrates :: CraneArm -> Ship -> Instruction -> Ship
moveCrates arm s (Move n f t) = IM.adjust (moving ++) t . IM.adjust (drop n) f $ s
    where
        moving = arm s f n

crane9000 :: CraneArm
crane9000 s col n = reverse . take n $ s IM.! col

crane9001 :: CraneArm
crane9001 s col n = take n $ s IM.! col

day05 :: CraneArm -> (Ship, [Instruction]) :~> String
day05 craneArm = MkSol
    { sParse = parseMaybeLenient parsePlanningSheet
    , sShow  = id
    , sSolve = traverse (preview _head) . IM.elems . uncurry (foldl' (moveCrates craneArm))
    }

day05a :: (Ship, [Instruction]) :~> String
day05a = day05 crane9000

day05b :: (Ship, [Instruction]) :~> String
day05b = day05 crane9001

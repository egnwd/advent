{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day11 (
    day11a
  , day11b
  ) where

import           AOC.Prelude

import Control.Monad.State
import Control.Lens
import Control.Lens.TH
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

data Monkey = Monkey
    { _mItems :: Seq.Seq Int
    , _mOp :: Int -> Int
    , _mTest :: Int
    , _mNext :: Int -> Int
    , _mInspected :: Int
    }

$(makeLenses ''Monkey)

parseMonkeys :: CharParser _
parseMonkeys = P.many parseMonkey
    where
        parseSection label p = P.space1 *> pTok label *> p <* optional P.newline
        parseMonkey = do
            n <- pTok "Monkey" *> pDecimal <* ":" <* P.newline
            items <- parseSection "Starting items:" (pDecimal `P.sepBy` ", ")
            op <- parseSection "Operation:" parseOperation
            (m, test) <- parseSection "Test:" parseTest
            return $ (n, Monkey (Seq.fromList items) op m test 0)
        parseOperation :: CharParser (Int -> Int)
        parseOperation = do
            pTok "new = old"
            op <- pTok ((*) <$ "*" <|> (+) <$ "+")
            other <- Just <$> pDecimal <|> Nothing <$ "old"
            return $ case other of
                       Just x -> \y -> y `op` x
                       Nothing -> \x -> x `op` x
        parseTest = do
            m <- pTok "divisible by" *> pDecimal <* P.newline
            let test x = x `mod` m == 0
            mtrue <- parseSection "If true:" (pTok "throw to monkey" *> pDecimal)
            mfalse <- parseSection "If false:" (pTok "throw to monkey" *> pDecimal)
            return $ (m, \n -> if test n then mtrue else mfalse)

monkeyRound :: IM.IntMap Monkey -> (IM.IntMap Monkey)
monkeyRound ms = fromJust $ execStateT round' ms
    where
        round' :: StateT (IM.IntMap Monkey) Maybe ()
        round' = do
            n <- gets IM.size
            forM_ [0..n-1] $ \x -> do
                ms <- get
                mon <- lift $ IM.lookup x ms
                let items = mon ^. mItems
                forM_ items $ \i -> do
                    ix x . mInspected += 1
                    let w = (mon ^. mOp $ i) `div` 3
                    let next = mon ^. mNext $ w
                    (ix next . mItems) %= (|> w)
                ix x . mItems .= mempty

monkeyRound2 :: IM.IntMap Monkey -> (IM.IntMap Monkey)
monkeyRound2 ms = fromJust $ execStateT round' ms
    where
        worry = product . fmap _mTest . IM.elems $ ms
        round' :: StateT (IM.IntMap Monkey) Maybe ()
        round' = do
            n <- gets IM.size
            forM_ [0..n-1] $ \x -> do
                ms <- get
                mon <- lift $ IM.lookup x ms
                let items = mon ^. mItems
                forM_ items $ \i -> do
                    ix x . mInspected += 1
                    let w = (mon ^. mOp $ i) `mod` worry
                    let next = mon ^. mNext $ w
                    (ix next . mItems) %= (|> w)
                ix x . mItems .= mempty

monkeyBusiness n = product
                 . take 2
                 . sortOn negate
                 . fmap _mInspected
                 . IM.elems
                 . head
                 . drop n

day11 :: Int -> (IM.IntMap Monkey -> IM.IntMap Monkey) -> IM.IntMap Monkey :~> Int
day11 n monkeyGame = MkSol
    { sParse = fmap IM.fromList . parseMaybeLenient parseMonkeys
    , sShow  = show
    , sSolve = Just . monkeyBusiness n . iterate monkeyGame
    }

day11a :: _ :~> _
day11a = day11 20 monkeyRound

day11b :: _ :~> _
day11b = day11 10000 monkeyRound2


{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day21
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 21.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day21 (
    day21a
  , day21b
  ) where

import           AOC.Prelude
import Control.Lens
import Data.Data
import Data.Data.Lens (uniplate)

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

data Op = Add | Sub | Mul | Div | Eql deriving (Eq, Ord, Data)

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Eql = "="

data MonkeyBusinessInput = YellI Int | MI Name Op Name | IX deriving (Eq, Show)

data MonkeyBusiness = Negate MonkeyBusiness | Yell Int | MM MonkeyBusiness Op MonkeyBusiness | MX deriving (Eq, Ord, Data)

instance Plated MonkeyBusiness where
  plate = uniplate

instance Show MonkeyBusiness where
    show (Yell n) = show n
    show (MM l op r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"
    show MX = "X"
    show (Negate m) = show (MM (Yell (-1)) Mul m)

type Name = String

eval Add = (+)
eval Sub = (-)
eval Mul = (*)
eval Div = (div)

parseMonkey :: CharParser (Name, MonkeyBusinessInput)
parseMonkey = do
    name <- pName <* pTok ":"
    mb <- (YellI <$> pDecimal) <|> (MI <$> pName <*> pOp <*> pName)
    return (name, mb)
        where
            pName = pTok $ P.takeWhileP (Just "name") isLetter
            pOp = (Add <$ pTok "+") <|> (Sub <$ pTok "-") <|> (Mul <$ pTok "*") <|> (Div <$ pTok "/")

getNumbers :: Map Name (MonkeyBusinessInput) -> Map Name Int
getNumbers ms = ms'
    where
        ms' = ms <&> go
        go (YellI n) = n
        go (MI l (eval->op) r) = (ms' M.! l) `op` (ms' M.! r)
        go IX = error "Not part of this challenge"

getEquations :: Map Name (MonkeyBusinessInput) -> Map Name MonkeyBusiness
getEquations ms = ms'
    where
        ms' = ms <&> go
        go :: MonkeyBusinessInput -> MonkeyBusiness
        go (YellI n) = Yell n
        go (MI l op r) = MM (ms' M.! l) op (ms' M.! r)
        go IX = MX

checkRoot :: Map Name MonkeyBusinessInput -> Map Name MonkeyBusiness -> Maybe MonkeyBusiness
checkRoot ms ns = do
    (MI l _ r) <- M.lookup "root" ms
    nl <- M.lookup l ns
    nr <- M.lookup r ns
    pure $ MM nl Eql nr

solve :: MonkeyBusiness -> Maybe Int
solve eq = case findX eq of
             (MM MX Eql (Yell n)) -> Just n
             (MM (Yell n) Eql MX) -> Just n
             _ -> Nothing
    where
        findX = rewrite (\x -> calc x <|> balance x)
        balance (MM (MM l Add (Yell r)) Eql o) = Just $ MM l Eql (MM o Sub (Yell r))
        balance (MM (MM l Sub (Yell r)) Eql o) = Just $ MM l Eql (MM o Add (Yell r))
        balance (MM (MM l Mul (Yell r)) Eql o) = Just $ MM l Eql (MM o Div (Yell r))
        balance (MM (MM l Div (Yell r)) Eql o) = Just $ MM l Eql (MM o Mul (Yell r))
        balance (MM (MM (Yell l) Add r) Eql o) = Just $ MM r Eql (MM o Sub (Yell l))
        balance (MM (MM (Yell l) Sub r) Eql o) = Just $ MM (Negate r) Eql (MM o Sub (Yell l))
        balance (MM (MM (Yell l) Mul r) Eql o) = Just $ MM r Eql (MM o Div (Yell l))
        balance (MM (Negate m) Eql (Yell o)) = Just $ MM m Eql (Yell (-o))
        balance _ = Nothing
        calc (MM (Yell l) op (Yell r)) = Just $ Yell ((eval op) l r)
        calc (Negate (Yell n)) = Just $ Yell (-n)
        calc _ = Nothing

mutate :: Map Name MonkeyBusinessInput -> Map Name MonkeyBusinessInput
mutate ms = ms & ix "humn" .~ IX

day21a :: _ :~> _
day21a = MkSol
    { sParse = fmap M.fromList . parseLines parseMonkey
    , sShow  = show
    , sSolve = (M.lookup "root") . getNumbers
    }

day21b :: Map Name (MonkeyBusinessInput) :~> _
day21b = MkSol
    { sParse = fmap M.fromList . parseLines parseMonkey
    , sShow  = show
    , sSolve = \ms -> solve <=< checkRoot ms . getEquations . mutate $ ms
    }

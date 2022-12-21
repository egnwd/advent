{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, TypeFamilies #-}

-- |
-- Module      : AOC.Challenge.Day21
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable

module AOC.Challenge.Day21 (
    day21a
  , day21b
  ) where

import           AOC.Solver                     ((:~>)(..))
import           AOC.Common                     (CharParser, pDecimal, pTok, parseLines)
import           Control.Lens                   (rewrite, (%~), (.~), Plated(..), ix, (&))
import           Data.Data
import           Data.Char                      (isLetter)
import           Data.Map                       (Map)
import           Control.Applicative            ((<|>), empty)
import           Data.Data.Lens                 (uniplate)
import           Data.Functor.Foldable          (ana)
import           Data.Functor.Foldable.TH       (makeBaseFunctor)

import qualified Data.Map                       as M
import qualified Text.Megaparsec                as P

type Name = String

data MonkeyBusiness
    = Yell Double
    | MonkeyBusiness :+: MonkeyBusiness
    | MonkeyBusiness :-: MonkeyBusiness
    | MonkeyBusiness :*: MonkeyBusiness
    | MonkeyBusiness :/: MonkeyBusiness
    | MonkeyBusiness :=: MonkeyBusiness
    | X
    deriving (Data)

$(makeBaseFunctor ''MonkeyBusiness)

instance Plated (MonkeyBusiness) where
  plate = uniplate

parseMonkey :: CharParser (Name, MonkeyBusinessF String)
parseMonkey = do
    name <- pName <* pTok ":"
    mb <- (YellF <$> pDecimal) <|> pOp
    return (name, mb)
        where
            pName = pTok $ P.takeWhileP (Just "name") isLetter
            pOp = do
                l <- pName
                op <- ((:+:$) <$ pTok "+") <|> ((:-:$) <$ pTok "-") <|> ((:*:$) <$ pTok "*") <|> ((:/:$) <$ pTok "/")
                r <- pName
                return $ op l r

findX :: MonkeyBusiness -> MonkeyBusiness
findX = rewrite (\x -> calc x <|> balance x)

balance, calc :: MonkeyBusiness -> Maybe MonkeyBusiness
balance = \case
    (l :+: (Yell r)) :=: o -> pure $ l :=: (o :-: (Yell r))
    (l :-: (Yell r)) :=: o -> pure $ l :=: (o :+: (Yell r))
    (l :*: (Yell r)) :=: o -> pure $ l :=: (o :/: (Yell r))
    (l :/: (Yell r)) :=: o -> pure $ l :=: (o :*: (Yell r))
    ((Yell l) :+: r) :=: o -> pure $ r :=: (o :-: (Yell l))
    ((Yell l) :*: r) :=: o -> pure $ r :=: (o :/: (Yell l))
    ((Yell l) :-: r) :=: o -> pure $ r :=: ((Yell l) :-: o)
    _                      -> empty

calc = \case
    (Yell l) :+: (Yell r) -> pure $ Yell (l + r)
    (Yell l) :-: (Yell r) -> pure $ Yell (l - r)
    (Yell l) :*: (Yell r) -> pure $ Yell (l * r)
    (Yell l) :/: (Yell r) -> pure $ Yell (l / r)
    _                     -> empty

getHumanYell :: MonkeyBusiness -> Maybe Int
getHumanYell = \case
     X :=: (Yell n) -> Just (round n)
     (Yell n) :=: X -> Just (round n)
     _              -> Nothing

getYell :: MonkeyBusiness -> Maybe Int
getYell = \case
    (Yell n) -> Just (round n)
    _        -> Nothing

retranslate :: Map Name (MonkeyBusinessF f) -> Map Name (MonkeyBusinessF f)
retranslate ms = ms & (ix "humn" .~ XF) & (ix "root" %~ \(l :+:$ r) -> l :=:$ r)

day21a :: Map Name (MonkeyBusinessF String) :~> Int
day21a = MkSol
    { sParse = fmap M.fromList . parseLines parseMonkey
    , sShow  = show
    , sSolve = \ms -> getYell . rewrite calc $ ana (ms M.!) "root"
    }

day21b :: Map Name (MonkeyBusinessF String) :~> Int
day21b = MkSol
    { sParse = fmap M.fromList . parseLines parseMonkey
    , sShow  = show
    , sSolve = \ms -> getHumanYell . findX $ ana ((retranslate ms) M.!) "root"
    }

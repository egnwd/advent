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
import Data.Functor.Const

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

data MonkeyBusinessInput = YellI Double | MI Name Op Name | IX deriving (Eq, Ord)

data MonkeyBusiness = Yell Double | MM MonkeyBusiness Op MonkeyBusiness | MX deriving (Eq, Ord, Data)

data MonkeyBusinessF f
    = YellM Double
    | MAdd (f (MonkeyBusinessF f)) (f (MonkeyBusinessF f))
    | MSub (f (MonkeyBusinessF f)) (f (MonkeyBusinessF f))
    | MMul (f (MonkeyBusinessF f)) (f (MonkeyBusinessF f))
    | MDiv (f (MonkeyBusinessF f)) (f (MonkeyBusinessF f))
    | MEql (f (MonkeyBusinessF f)) (f (MonkeyBusinessF f))
    | X

deriving instance Eq (MonkeyBusinessF Identity)
deriving instance Ord (MonkeyBusinessF Identity)
deriving instance Data (MonkeyBusinessF Identity)

instance Plated (MonkeyBusinessF Identity) where
  plate = uniplate

instance Plated MonkeyBusiness where
  plate = uniplate

type Name = String

parseMonkey :: CharParser (Name, MonkeyBusinessInput)
parseMonkey = do
    name <- pName <* pTok ":"
    mb <- (YellI <$> pDecimal) <|> (MI <$> pName <*> pOp <*> pName)
    return (name, mb)
        where
            pName = pTok $ P.takeWhileP (Just "name") isLetter
            pOp = (Add <$ pTok "+") <|> (Sub <$ pTok "-") <|> (Mul <$ pTok "*") <|> (Div <$ pTok "/")

parseMonkeyF :: CharParser (Name, MonkeyBusinessF (Const Name))
parseMonkeyF = do
    name <- pName <* pTok ":"
    mb <- (YellM <$> pDecimal) <|> pOp
    return (name, mb)
        where
            pName = pTok $ P.takeWhileP (Just "name") isLetter
            pOp = do
                l <- pName
                op <- (MAdd <$ pTok "+") <|> (MSub <$ pTok "-") <|> (MMul <$ pTok "*") <|> (MDiv <$ pTok "/")
                r <- pName
                return $ op (Const l) (Const r)

getEquations :: Map Name MonkeyBusinessInput -> Map Name MonkeyBusiness
getEquations ms = ms'
    where
        ms' = ms <&> go
        go :: MonkeyBusinessInput -> MonkeyBusiness
        go (YellI n) = Yell n
        go (MI l op r) = MM (ms' M.! l) op (ms' M.! r)
        go IX = MX

getEquationsF :: Map Name (MonkeyBusinessF (Const Name)) -> Map Name (MonkeyBusinessF Identity)
getEquationsF ms = ms'
    where
        ms' = ms <&> go
        go :: MonkeyBusinessF (Const Name) -> MonkeyBusinessF Identity
        go (MAdd (getConst->l) (getConst->r)) = MAdd (get l) (get r)
        go (MSub (getConst->l) (getConst->r)) = MSub (get l) (get r)
        go (MMul (getConst->l) (getConst->r)) = MMul (get l) (get r)
        go (MDiv (getConst->l) (getConst->r)) = MDiv (get l) (get r)
        go (MEql (getConst->l) (getConst->r)) = MEql (get l) (get r)
        go X = X
        go (YellM n) = YellM n
        get :: Name -> Identity (MonkeyBusinessF Identity)
        get = pure . (ms' M.!)

checkRoot :: Map Name MonkeyBusinessInput -> Map Name MonkeyBusiness -> Maybe MonkeyBusiness
checkRoot ms ns = do
    (MI l _ r) <- M.lookup "root" ms
    nl <- M.lookup l ns
    nr <- M.lookup r ns
    pure $ MM nl Eql nr

solve :: MonkeyBusiness -> Maybe Double
solve eq = case findX eq of
             (MM MX Eql (Yell n)) -> Just n
             (MM (Yell n) Eql MX) -> Just n
             _ -> Nothing
    where
        findX = rewrite (\x -> calc x <|> balance x)
        balance = \case
            MM (MM l Add (Yell r)) Eql o -> pure $ MM l Eql (MM o Sub (Yell r))
            MM (MM l Sub (Yell r)) Eql o -> pure $ MM l Eql (MM o Add (Yell r))
            MM (MM l Mul (Yell r)) Eql o -> pure $ MM l Eql (MM o Div (Yell r))
            MM (MM l Div (Yell r)) Eql o -> pure $ MM l Eql (MM o Mul (Yell r))
            MM (MM (Yell l) Add r) Eql o -> pure $ MM r Eql (MM o Sub (Yell l))
            MM (MM (Yell l) Sub r) Eql o -> pure $ MM r Eql (MM (Yell l) Sub o)
            MM (MM (Yell l) Mul r) Eql o -> pure $ MM r Eql (MM o Div (Yell l))
            _                            -> empty
        calc = \case
            MM (Yell l) Add (Yell r) -> pure $ Yell (l + r)
            MM (Yell l) Sub (Yell r) -> pure $ Yell (l - r)
            MM (Yell l) Mul (Yell r) -> pure $ Yell (l * r)
            MM (Yell l) Div (Yell r) -> pure $ Yell (l / r)
            _                       -> empty

solveF :: MonkeyBusinessF Identity -> Maybe Double
solveF eq = case findX eq of
             (MEql (Identity X) (Identity (YellM n))) -> Just $ n
             (MEql (Identity (YellM n)) (Identity X)) -> Just $ n
             _ -> Nothing
    where
        findX = rewrite (\x -> calc x <|> balance x)
        balance = \case
            MEql (Identity (MAdd l (Identity (YellM r)))) o -> pure $ MEql l (pure (MSub o (pure (YellM r))))
            MEql (Identity (MSub l (Identity (YellM r)))) o -> pure $ MEql l (pure (MAdd o (pure (YellM r))))
            MEql (Identity (MMul l (Identity (YellM r)))) o -> pure $ MEql l (pure (MDiv o (pure (YellM r))))
            MEql (Identity (MDiv l (Identity (YellM r)))) o -> pure $ MEql l (pure (MMul o (pure (YellM r))))
            MEql (Identity (MAdd (Identity (YellM l)) r)) o -> pure $ MEql r (pure (MSub o (pure (YellM l))))
            MEql (Identity (MSub (Identity (YellM l)) r)) o -> pure $ MEql r (pure (MSub (pure (YellM l)) o))
            MEql (Identity (MMul (Identity (YellM l)) r)) o -> pure $ MEql r (pure (MDiv o (pure (YellM l))))
            _                            -> empty
        calc = \case
            MAdd (Identity (YellM l)) (Identity (YellM r)) -> pure $ YellM (l + r)
            MSub (Identity (YellM l)) (Identity (YellM r)) -> pure $ YellM (l - r)
            MMul (Identity (YellM l)) (Identity (YellM r)) -> pure $ YellM (l * r)
            MDiv (Identity (YellM l)) (Identity (YellM r)) -> pure $ YellM (l / r)
            _                       -> empty

retranslate :: Map Name MonkeyBusinessInput -> Map Name MonkeyBusinessInput
retranslate ms = ms & (ix "humn" .~ IX) & (ix "root" %~ \(MI l _ r) -> MI l Eql r)

retranslateF :: Map Name (MonkeyBusinessF f) -> Map Name (MonkeyBusinessF f)
retranslateF ms = ms & (ix "humn" .~ X) & (ix "root" %~ \(MAdd l r) -> MEql l r)

day21a :: _ :~> Int
day21a = MkSol
    { sParse = fmap M.fromList . parseLines parseMonkeyF
    , sShow  = show
    , sSolve = fmap round . solveF <=< fmap (MEql (pure X) . pure) . (M.lookup "root") . getEquationsF
    }

day21b :: Map Name (MonkeyBusinessF (Const Name)) :~> Int
day21b = MkSol
    { sParse = fmap M.fromList . parseLines parseMonkeyF
    , sShow  = show
    , sSolve = fmap round . solveF <=< M.lookup "root" . getEquationsF . retranslateF
    }

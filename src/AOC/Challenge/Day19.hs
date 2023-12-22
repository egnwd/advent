{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day19
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 19.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day19 (
    day19a
  , day19b
                           , lx, getLens
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
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
import Data.Bitraversable
import Data.Interval ((<=..<=), (<=..<), (<..<=))
import qualified Data.Interval as I
import qualified Data.IntervalSet as IS

import Control.Lens

data Rating a = XMAS { _x :: a
                     , _m :: a
                     , _a :: a
                     , _s :: a
                     } deriving (Show, Foldable, Functor)

type Workflow = (Name, [Rule])
data Rule = Conditional Condition Label | Automatic Label deriving (Show)
data Condition = IF Char (I.Interval Int)
data Label = Accepted | Rejected | Name Name deriving (Eq, Show)
type Name = String

instance Show Condition where
    show (IF l r) = l : show r

lx, lm, la, ls :: Lens' (Rating a) a
lx f (XMAS{..}) = (\x' -> XMAS x' _m _a _s) <$> f _x
lm f (XMAS{..}) = (\m' -> XMAS _x m' _a _s) <$> f _m
la f (XMAS{..}) = (\a' -> XMAS _x _m a' _s) <$> f _a
ls f (XMAS{..}) = (\s' -> XMAS _x _m _a s') <$> f _s

getLens :: Char -> Lens' (Rating a) a
getLens = \case
    'x' -> lx
    'm' -> lm
    'a' -> la
    's' -> ls

workflow :: CharParser (Workflow)
workflow = (,) <$> P.takeWhileP Nothing isLower <*> P.between (P.char '{') (P.char '}') (rule `P.sepBy` (P.char ','))
    where
        rule = (P.try cond) <|> auto
        cond = do
            n <- P.choice [(P.char 'x'), P.char 'm', P.char 'a', P.char 's']
            op <- ((I.NegInf <=..<) <$ P.char '<') <|> ((<..<= I.PosInf) <$ P.char '>')
            i <- pDecimal
            l <- P.char ':' *> label
            return $ Conditional (IF n (op i)) l
        auto = Automatic <$> label

        label = (Accepted <$ P.char 'A') <|> (Rejected <$ P.char 'R') <|> (Name <$> P.takeWhileP Nothing isLower)

rating =  P.between (P.char '{') (P.char '}') $ do
    x <- "x=" *> pDecimal <* ","
    m <- "m=" *> pDecimal <* ","
    a <- "a=" *> pDecimal <* ","
    s <- "s=" *> pDecimal
    return $ XMAS x m a s

run start workflows = sum . map sum . filter (go (workflows M.! start))
    where
        go :: [Rule] -> Rating Int -> Bool
        go (r : rs) rt = case r of
            (Automatic Accepted) -> True
            (Automatic Rejected) -> False
            (Automatic (Name n)) -> go (workflows M.! n) rt
            (Conditional (IF xmas p) l) -> if I.member (rt ^. (getLens xmas)) p then go [Automatic l] rt else go rs rt

starterFor10 = I.Finite 1 <=..<= I.Finite 4000

run2 start workflows = go (workflows M.! start) [(XMAS starterFor10 starterFor10 starterFor10 starterFor10)]
    where
        go (r : rs) rts = case r of
            (Automatic Accepted) -> rts
            (Automatic Rejected) -> []
            (Automatic (Name n)) -> go (workflows M.! n) rts
            (Conditional (IF xmas p) l) -> go [Automatic l] (yays rts xmas p) ++ go rs (nays rts xmas p)

yays :: [Rating (I.Interval Int)] -> Char -> I.Interval Int -> [Rating (I.Interval Int)]
yays rts xmas p = concatMap (\rt -> map (\i -> rt & getLens xmas .~ i) newIs) rts
    where
        newIs = IS.toList . IS.intersection (IS.fromList . map (view (getLens xmas)) $ rts) $ IS.singleton p

nays :: [Rating (I.Interval Int)] -> Char -> I.Interval Int -> [Rating (I.Interval Int)]
nays rts xmas p = concatMap (\rt -> map (\i -> rt & getLens xmas .~ i) newIs) rts
    where
        newIs = IS.toList . IS.difference (IS.fromList . map (view (getLens xmas)) $ rts) $ IS.singleton p

day19a :: (Map Name [Rule], [Rating Int]) :~> _
day19a = MkSol
    { sParse = bitraverse (fmap M.fromList . parseLines workflow) (parseLines rating) <=< listTup . splitOn "\n\n"
    , sShow  = show
    , sSolve = Just . uncurry (run "in")
    }

day19b :: _ :~> _
day19b = MkSol
    { sParse = sParse day19a
    , sShow  = show
    , sSolve = Just . sum . map (product . fmap I.width) . run2 "in" . fst
    }

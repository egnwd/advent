{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

-- |
-- Module      : AOC.Challenge.Day18
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 18.

module AOC.Challenge.Day18 (
    day18a
  , day18b
  ) where

import AOC.Solver               ((:~>)(..))
import AOC.Common               (parseLines, CharParser, pDecimal)
import Control.Applicative      ((<|>))
import Control.Comonad          (extract)
import Control.Comonad.Cofree   (Cofree(..))
import Control.Monad            ((<=<), guard)
import Data.Foldable            (asum, toList)
import Data.Functor.Foldable    (histo, para, cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List                (tails)
import Data.List.NonEmpty       (NonEmpty(..), nonEmpty)
import Data.Semigroup           (Max(..), getMax)
import Text.Megaparsec          (between)

-- ^ Snail Number Types & Instances

data SnailNum = SN Int | SP SnailNum SnailNum deriving (Eq)
data Todo = NoWork | PlaceLeft Int | PlaceRight Int

$(makeBaseFunctor ''SnailNum)

instance Show SnailNum where
    show (SN n) = show n
    show (SP l r) = show [l,r]

pattern (:@:) :: forall a. Int -> Int -> Cofree SnailNumF a
pattern nl :@: nr <- (_ :< SPF (_ :< (SNF nl)) (_ :< (SNF nr)))

out :: Cofree SnailNumF a -> SnailNum
out (_ :< SNF n) = SN n
out (_ :< SPF l r) = SP (out l) (out r)

-- ^ Parsing

parsePair, parseNumber, parseSnail :: CharParser SnailNum
parsePair = between "[" "]" $ SP <$> parseSnail <*> ("," *> parseSnail)
parseNumber = SN <$> pDecimal
parseSnail = parseNumber <|> parsePair

-- ^ Snail Maths

(@+) :: SnailNum -> SnailNum -> SnailNum
a @+ b = applySnailRules (SP a b)
    where
        applySnailRules n = maybe n applySnailRules (asum [explode n, split n])

-- ^ Add @n@ onto the leftmost element
(^@+) :: Int -> SnailNum -> SnailNum
(^@+) n = para go
    where
        go :: SnailNumF (SnailNum, SnailNum) -> SnailNum
        go (SNF sn) = SN (sn+n)
        go (SPF (_, l) (r, _)) = SP l r

-- ^ Add @n@ onto the rightmost element
(@+^) :: Int -> SnailNum -> SnailNum
(@+^) n = para go
    where
        go :: SnailNumF (SnailNum, SnailNum) -> SnailNum
        go (SNF sn) = SN (sn+n)
        go (SPF (l, _) (_, r)) = SP l r

explode :: SnailNum -> Maybe SnailNum
explode inN = snd <$> histo go inN 0
    where
        go :: SnailNumF (Cofree SnailNumF (Int -> Maybe (Todo, SnailNum)))
           -> Int -> Maybe (Todo, SnailNum)
        go (SNF _) _ = Nothing
        go (SPF (nl :@: nr) r) 3 = Just (PlaceLeft nl, SP (SN 0) (nr ^@+ out r))
        go (SPF l (nl :@: nr)) 3 = Just (PlaceRight nr, SP (nl @+^ out l) (SN 0))
        go (SPF l r) d = asum [explodeLeft <$> extract l (d+1), explodeRight <$> extract r (d+1)]
              where
                explodeLeft (PlaceRight nr, sn) = (NoWork, SP sn (nr ^@+ out r))
                explodeLeft (todo, sn) = (todo, SP sn (out r))

                explodeRight (PlaceLeft nl, sn) = (NoWork, SP (nl @+^ out l) sn)
                explodeRight (todo, sn) = (todo, SP (out l) sn)

split :: SnailNum -> Maybe SnailNum
split = histo go
    where
        go :: SnailNumF (Cofree SnailNumF (Maybe SnailNum)) -> Maybe SnailNum
        go (SNF n) = do
            guard $ n >= 10
            let n' = (fromIntegral n / 2) :: Double
            pure $ SN (floor n') `SP` SN (ceiling n')
        go (SPF l r) = asum [(`SP` out r) <$> extract l, SP (out l) <$> extract r]

magnitude :: SnailNum -> Int
magnitude = cata go
    where
        go (SNF n) = n
        go (SPF l r) = (3 * l) + (2 * r)

maxMagnitude :: NonEmpty SnailNum -> Int
maxMagnitude ns = getMax . foldMap (Max . magnitude) $ do
    (x:ys) <- tails (toList ns)
    y <- ys
    [x @+ y, y @+ x]

day18a :: NonEmpty SnailNum :~> Int
day18a = MkSol
    { sParse = nonEmpty <=< parseLines parsePair
    , sShow  = show
    , sSolve = Just . magnitude . foldl1 (@+)
    }

day18b :: NonEmpty SnailNum :~> Int
day18b = MkSol
    { sParse = nonEmpty <=< parseLines parsePair
    , sShow  = show
    , sSolve = Just . maxMagnitude
    }

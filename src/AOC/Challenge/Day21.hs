{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day21
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 21.

module AOC.Challenge.Day21 (
    day21a
  , day21b
  ) where

import AOC.Solver
import AOC.Common
import Data.Foldable
import Control.Lens
import qualified Text.Megaparsec.Char as P

type Cost = Int

data Stats = S
    { _hitPoints :: Float
    , _damage :: Float
    , _armor :: Float
    } deriving (Show)

$(makeLenses ''Stats)

data Item = Item
    { _itemCost :: Int
    , _itemDamage :: Float
    , _itemArmor :: Float
    } deriving (Eq)

instance Show Item where
    show (Item c d a) = show (c, d, a)

$(makeLenses ''Item)

instance Semigroup Item where
    a <> b = Item (a ^. itemCost + b ^. itemCost)
                  (a ^. itemDamage + b ^. itemDamage)
                  (a ^. itemArmor + b ^. itemArmor)

instance Monoid Item where
    mempty = Item 0 0 0
    mappend = (<>)

dagger, shortsword, warhammer, longsword, greataxe :: Item
dagger     = Item   8 4 0
shortsword = Item  10 5 0
warhammer  = Item  25 6 0
longsword  = Item  40 7 0
greataxe   = Item  74 8 0

leather, chainmail, splintmail, bandedmail, platemail :: Item
leather    = Item  13 0 1
chainmail  = Item  31 0 2
splintmail = Item  53 0 3
bandedmail = Item  75 0 4
platemail  = Item 102 0 5

damage1, damage2, damage3, defense1, defense2, defense3 :: Item
damage1    = Item  25 1 0
damage2    = Item  50 2 0
damage3    = Item 100 3 0
defense1   = Item  20 0 1
defense2   = Item  40 0 2
defense3   = Item  80 0 3

weapons, armors, rings :: [Item]
weapons = [dagger, shortsword, warhammer, longsword, greataxe]
armors = [leather, chainmail, splintmail, bandedmail, platemail]
rings = [damage1, damage2, damage3, defense1, defense2, defense3]

possibilities :: [Item]
possibilities = mconcat [a, b, c, d, e, f]
    where
        a = map fold . sequence $ [weapons]
        b = map fold . sequence $ [weapons, armors]
        c = map fold . sequence $ [weapons, rings]
        d = map fold . sequence $ [weapons, armors, rings]
        e = map fold . sequence $ [weapons, armors, rings, rings]
        f = map fold . sequence $ [weapons, rings, rings]

simulateFight :: Stats -> Stats -> Bool
simulateFight p b = hits p b <= hits b p
    where
        hits :: Stats -> Stats -> Int
        hits x y = ceiling (y ^. hitPoints / max 1 (x ^. damage - y ^. armor))

parseStats :: CharParser Stats
parseStats = do
    hp <- "Hit Points: " *> pDecimal <* P.space
    d <- "Damage: " *> pDecimal <* P.space
    a <- "Armor: " *> pDecimal <* P.space
    return $ S hp d a

parta, partb :: Stats -> Cost
parta = solve minimum id
partb = solve maximum not

solve :: ([Cost] -> Cost) -> (Bool -> Bool) -> Stats -> Cost
solve mx f boss = mx . map fst . filter (f . snd) . map (\p -> (p ^. itemCost, simulateFight (getPlayer p) boss)) $ possibilities
    where
        getPlayer p = S 100 (p ^. itemDamage) (p ^. itemArmor)

day21a :: Stats :~> Cost
day21a = MkSol
    { sParse = parseMaybeLenient parseStats
    , sShow  = show
    , sSolve = Just . parta
    }

day21b :: Stats :~> Cost
day21b = MkSol
    { sParse = parseMaybeLenient parseStats
    , sShow  = show
    , sSolve = Just . partb
    }

-- |
-- Module      : AOC.Challenge.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.

module AOC.Challenge.Day15 (
    day15a
  , day15b
  ) where

import AOC.Solver ((:~>)(..))
import AOC.Common (parseLines, CharParser, pTok, pDecimal, pWord)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Lens
import Control.Monad

data Ingredient = Ingredient
    { _cCapacity :: Int
    , _cDurability :: Int
    , _cFlavor :: Int
    , _cTexture :: Int
    , _cCalories :: Int
    } deriving (Eq, Show)

$(makeLenses ''Ingredient)

-- Sprinkles: capacity 2, durability 0, flavor -2, texture 0, calories 3
parseIngredient :: CharParser Ingredient
parseIngredient = Ingredient <$> (name *> property) <*> property <*> property <*> property <*> property
        where
            name = takeWhileP Nothing (/=':') <* pTok (char ':')
            property = pWord *> pTok pDecimal <* optional (pTok (char ','))

experimentRecipes :: ([Int] -> Bool) -> [Ingredient] -> Maybe [Int]
experimentRecipes f is = do
    let as = attempts (length is) 100
    guard $ (not . null) as
    pure $ map (`scoreRecipe` is) . filter f $ as
        where
            attempts :: Int -> Int -> [[Int]]
            attempts 1 l = [[l]]
            attempts n l = do
                guard $ n > 0
                a <- [0..l]
                as <- attempts (n-1) (l-a)
                pure $ a:as

allCookies :: [Int] -> Bool
allCookies = const True

calorieControlledCookies :: [Ingredient] -> [Int] -> Bool
calorieControlledCookies is ps = (== 500) . sum $ zipWith (\i n -> (i ^. cCalories) * n) is ps

portionIngredient :: Int -> Ingredient -> Ingredient
portionIngredient n i =
    i & cCapacity   *~ n
      & cDurability *~ n
      & cFlavor     *~ n
      & cTexture    *~ n

scoreRecipe :: [Int] -> [Ingredient] -> Int
scoreRecipe ps = product . map (max 0) . foldr1 applyIngredient . map properties . zipWith portionIngredient ps

properties :: Ingredient -> [Int]
properties i = [i ^. cCapacity, i ^. cDurability, i ^. cFlavor, i ^. cTexture]

applyIngredient :: [Int] -> [Int] -> [Int]
applyIngredient = zipWith (+)

day15a :: _ :~> _
day15a = MkSol
    { sParse = parseLines parseIngredient
    , sShow  = show
    , sSolve = fmap maximum . experimentRecipes allCookies
    }

day15b :: _ :~> _
day15b = MkSol
    { sParse = parseLines parseIngredient
    , sShow  = show
    , sSolve = \is -> fmap maximum . experimentRecipes (calorieControlledCookies is) $ is
    }

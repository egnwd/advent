-- |
-- Module      : AOC.Challenge
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Meant to be a place to include common functionality used across
-- different parts in the challenge.
--


module AOC.Common (
                    (!?)
                  , CharParser
                  , Parser
                  , pSpace
                  , pWord
                  , pDecimal
                  , pTok
                  , parseMaybeLenient
                  , parseOrFail
                  , parseLines
                  , parseLinesOrError
                  , fixedPoint
                  , freqs
                  , lookupFreq
                  , odds
                  , evens
                  , countTrue
                  , Point
                  , parseAsciiMap
                  , parseAsciiSet
                  , displayAsciiMap
                  , displayAsciiSet
                  , inBoundingBox
                  , boundingBox
                  ) where

import           Control.Applicative
import           Data.Monoid
import           Data.Char
import           Data.Foldable
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Maybe
import           Data.Void
import           Linear
import           AOC.Util
import           Control.Lens
import           Data.Set.Lens
import           Data.Map.Lens
import           Data.Tuple.Strict
import qualified Data.Map.NonEmpty          as NEM
import qualified Data.Set.NonEmpty          as NES
import qualified Data.Map                   as M
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as PL
import qualified Data.Text                  as T

(!?) :: [a] -> Int -> Maybe a
[]     !? _ = Nothing
(x:_ ) !? 0 = Just x
(x:xs) !? n = x `seq` (xs !? (n - 1))

type CharParser = P.Parsec Void String
type Parser = P.Parsec Void T.Text

-- | Doesn't fail if the whole imput is not consumed
parseMaybeLenient :: P.Parsec Void s a -> s -> Maybe a
parseMaybeLenient p = eitherToMaybe . P.parse p "parseMaybeLenient"

parseOrFail :: (P.Stream s, P.ShowErrorComponent e) => P.Parsec e s a -> s -> a
parseOrFail p = either (error . P.errorBundlePretty) id . P.parse p "parseOrFail"

pSpace :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s ()
pSpace = P.skipMany (P.char ' ')

pTok :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s a -> P.Parsec e s a
pTok p = p <* pSpace

pWord :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s String
pWord = pTok $ P.many (P.satisfy (not . isSpace))

pDecimal :: (P.Stream s, P.Token s ~ Char, Ord e, Num a) => P.Parsec e s a
pDecimal = PL.signed P.space PL.decimal

parseLines :: P.Parsec Void String a -> String -> Maybe [a]
parseLines p = Just . mapMaybe (parseMaybeLenient p) . lines

parseLinesOrError :: P.Parsec Void String a -> String -> Maybe [a]
parseLinesOrError p = Just . map (parseOrFail p) . lines

-- | Repeat a function until you get the same result twice in a row
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f = go
  where
    go !x
        | x == y    = x
        | otherwise = go y
      where
        y = f x

-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList

-- | Look up a count from a frequency map, defaulting to zero if item is
-- not found
lookupFreq :: Ord a => a -> Map a Int -> Int
lookupFreq = M.findWithDefault 0

odds :: [a] -> [a]
odds [] = []
odds (x:xs) = x:evens xs

evens :: [a] -> [a]
evens [] = []
evens (_:xs) = odds xs

-- | Count the number of items in a container where the predicate is true.
countTrue :: Foldable f => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

-- https://github.com/mstksg/advent-of-code-2020/blob/165461e51f991ac44bc9f8acc5c4e17caf83c13b/src/AOC/Common/Point.hs

type Point = V2 Int

boundingBox :: (Foldable1 f, Applicative g, Ord a) => f (g a) -> V2 (g a)
boundingBox = (\(T2 (Ap mn) (Ap mx)) -> V2 (getMin <$> mn) (getMax <$> mx))
            . foldMap1 (\p -> T2 (Ap (Min <$> p)) (Ap (Max <$> p)))

parseAsciiMap
    :: (Char -> Maybe a)
    -> String
    -> Map Point a
parseAsciiMap f = toMapOf (asciiGrid <. folding f)

parseAsciiSet
    :: (Char -> Bool)
    -> String
    -> Set Point
parseAsciiSet f = setOf (asciiGrid . filtered f . asIndex)

asciiGrid :: IndexedTraversal Point String [a] Char a
asciiGrid = conjoined traverse $ \z ->
      sequenceA
    . concat
    . zipWith (\y -> zipWith (\x -> indexed z (V2 x y :: Point)) [0..]) [0..]
    . lines

displayAsciiMap
    :: Char             -- ^ default tile
    -> Map Point Char   -- ^ tile map
    -> String
displayAsciiMap d (NEM.IsNonEmpty mp) = unlines
    [ [ NEM.findWithDefault d (V2 x y) mp
      | x <- [xMin .. xMax]
      ]
    | y <- [yMin .. yMax]
    ]
  where
    V2 xMin yMin `V2` V2 xMax yMax = boundingBox $ NEM.keysSet mp
displayAsciiMap _ _ = ""

displayAsciiSet
    :: Char      -- ^ missing tile
    -> Char      -- ^ present tile
    -> Set Point -- ^ tile set
    -> String
displayAsciiSet x y = displayAsciiMap x . M.fromSet (const y)

inBoundingBox
    :: (Applicative g, Foldable g, Ord a)
    => V2 (g a)
    -> g a
    -> Bool
inBoundingBox (V2 mn mx) x = and $ go <$> x <*> mn <*> mx
  where
    go x' mn' mx' = x' >= mn' && x' <= mx'

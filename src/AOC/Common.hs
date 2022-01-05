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
                  , (-?)
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
                  , hexDigit
                  , binDigit
                  , hexToBin
                  , fixedPoint
                  , indexedFixedPoint
                  , loopEither
                  , loopMaybe
                  , foldMapKeysWith
                  , freqs
                  , lookupFreq
                  , revFreq
                  , odds
                  , evens
                  , sequenceTuple
                  , countTrue
                  , pickUnique
                  , lineTo
                  , module AOC
                  ) where

import           Data.Char
import           Data.Foldable
import           Data.Map (Map)
import           Data.IntMap (IntMap)
import           Data.Set (Set)
import           Data.Set.NonEmpty (NESet)
import           Data.List
import           Data.Maybe
import           Data.Traversable
import           Data.Tuple
import           Data.Bifunctor
import           Data.Void
import           AOC.Util
import           Control.Monad.State
import           Control.Applicative
import           Linear
import           GHC.TypeNats
import           Control.Lens
import           Data.Finite
import           AOC.Common.Point           as AOC
import           AOC.Common.Search          as AOC
import qualified Data.Map                   as M
import qualified Data.IntMap                as IM
import qualified Data.Set                   as S
import qualified Data.Set.NonEmpty          as NES
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as PL
import qualified Data.Text                  as T

-- Some fns from: https://github.com/mstksg/advent-of-code-2020/blob/165461e51f991ac44bc9f8acc5c4e17caf83c13b/src/AOC/Common.hs

(!?) :: [a] -> Int -> Maybe a
[]     !? _ = Nothing
(x:_ ) !? 0 = Just x
(x:xs) !? n = x `seq` (xs !? (n - 1))

(-?) :: (Applicative f, Num c) => f c -> f c -> f c
(-?) = liftA2 (-)

type CharParser = P.Parsec Void String
type Parser = P.Parsec Void T.Text

-- | Doesn't fail if the whole input is not consumed
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

hexDigit :: Prism' Char (Finite 16)
hexDigit = baseDigit

binDigit :: Prism' Char (Finite 2)
binDigit = baseDigit

baseDigit :: (KnownNat n) => Prism' Char (Finite (n :: Nat))
baseDigit = prism' _to _from
  where
    _to              = intToDigit . fromIntegral
    _from c
      | isHexDigit c = Just (finite (fromIntegral (digitToInt c)))
      | otherwise    = Nothing

hexToBin :: String -> Maybe String
hexToBin = fmap (map (review binDigit) . concat) . traverse hexToBin' <=< traverse (preview hexDigit)
  where
    hexToBin' :: Finite 16 -> Maybe [Finite 2]
    hexToBin' = traverse packFinite <=< toBinDigits
    toBinDigits = \case
      0  -> pure [0,0,0,0]
      1  -> pure [0,0,0,1]
      2  -> pure [0,0,1,0]
      3  -> pure [0,0,1,1]
      4  -> pure [0,1,0,0]
      5  -> pure [0,1,0,1]
      6  -> pure [0,1,1,0]
      7  -> pure [0,1,1,1]
      8  -> pure [1,0,0,0]
      9  -> pure [1,0,0,1]
      10 -> pure [1,0,1,0]
      11 -> pure [1,0,1,1]
      12 -> pure [1,1,0,0]
      13 -> pure [1,1,0,1]
      14 -> pure [1,1,1,0]
      15 -> pure [1,1,1,1]
      _  -> Nothing


-- | Repeat a function until you get the same result twice in a row
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f = go
  where
    go !x
        | x == y    = x
        | otherwise = go y
      where
        y = f x

indexedFixedPoint :: Eq a => (a -> a) -> a -> (Int, a)
indexedFixedPoint f = go 1
  where
    go idx !x
        | x == y    = (idx, x)
        | otherwise = go (idx+1) y
      where
        y = f x

loopEither
    :: (a -> Either r a)
    -> a
    -> r
loopEither f = go
  where
    go !x = case f x of
      Left  r  -> r
      Right !y -> go y

loopMaybe
    :: (a -> Maybe a)
    -> a
    -> (Int, a)
loopMaybe f = go 0
  where
    go n !x = case f x of
      Nothing -> (n, x)
      Just !y -> go (n+1) y

foldMapKeysWith :: (Ord k2) => (a -> a -> a) -> (k -> [k2]) -> M.Map k a -> M.Map k2 a
foldMapKeysWith fWith f = M.fromListWith fWith . foldMap (\(k,a) -> map (,a) . f $ k) . M.toList

-- | Build a frequency map
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . map (,1) . toList

-- | Look up a count from a frequency map, defaulting to zero if item is
-- not found
lookupFreq :: Ord a => a -> Map a Int -> Int
lookupFreq = M.findWithDefault 0

revFreq :: (Foldable f, Ord a) => f a -> IntMap (NESet a)
revFreq = IM.fromListWith (<>)
        . map (swap . first NES.singleton)
        . M.toList
        . freqs

odds :: [a] -> [a]
odds [] = []
odds (x:xs) = x:evens xs

evens :: [a] -> [a]
evens [] = []
evens (_:xs) = odds xs

sequenceTuple :: (Maybe a, Maybe b) -> Maybe (a,b)
sequenceTuple (Just a, Just b) = Just (a, b)
sequenceTuple _ = Nothing

-- | Count the number of items in a container where the predicate is true.
countTrue :: Foldable f => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

-- | Picks unique combinations of elements from the input as maps
pickUnique :: (Ord k, Ord a) => [(k, Set a)] -> [Map k a]
pickUnique mp = flip evalStateT S.empty $ do
    fmap M.fromList . for opts . traverse $ \poss -> do
      seen <- get
      pick <- lift $ S.toList (poss `S.difference` seen)
      pick <$ modify (S.insert pick)
  where
    opts = sortOn (S.size . snd) mp

lineTo :: V2 Point -> [Point]
lineTo (V2 p0 p1) = [p0 + t *^ step | t <- [0 .. gcf]]
  where
    d@(V2 dx dy) = p1 - p0
    gcf          = gcd dx dy
    step         = (`div` gcf) <$> d


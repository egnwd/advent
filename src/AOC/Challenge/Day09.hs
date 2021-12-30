-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.

module AOC.Challenge.Day09 (
    day09a
  , day09b
  ) where

import AOC.Solver           ((:~>)(..))
import AOC.Common           (CharParser, pDecimal)
import Data.Functor         (($>))
import Data.Monoid          (Sum(..), getSum)
import Data.Semigroup       (stimes)
import Control.Monad        (when)
import Control.Monad.State  (evalState)
import Control.Lens         (use, makeLenses, (.=), (%=))
import Text.Megaparsec      (takeP, takeRest, parseMaybe)
import Text.Megaparsec.Char (char)

data DecompressState = DS { _input :: String, _output :: Sum Int }

$(makeLenses ''DecompressState)

type Decompression = String -> Sum Int

version1Decompression, version2Decompression :: Decompression
version1Decompression = Sum . length
version2Decompression = decompress version2Decompression

decompress :: Decompression -> Decompression
decompress d in0 = evalState (go >> use output) (DS in0 mempty)
    where
        go = findMarker >>= (`when` go)
        findMarker = do
            inp <- use input
            case parseMaybe parser inp of
              Nothing -> use input >>= \case
                    [] -> pure False
                    _  -> (input %= tail) >> (output %= (<> Sum 1)) $> True
              Just (r, out, rs) -> (input .= rs) >> (output %= (<> stimes r (d out))) $> True

parser :: CharParser (Int, String, String)
parser = do
    n <- char '(' *> pDecimal
    r <- char 'x' *> pDecimal <* char ')'
    out <- takeP Nothing n
    rest <- takeRest
    pure (r, out, rest)

day09 :: Decompression -> String :~> Int
day09 d = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just . getSum . decompress d
    }

day09a :: String :~> Int
day09a = day09 version1Decompression

day09b :: String :~> Int
day09b = day09 version2Decompression

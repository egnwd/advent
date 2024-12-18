{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day17 (
    day17a
  , day17b
  ) where

import           AOC.Solver ((:~>)(..))
import           AOC.Util   (eitherToMaybe)
import           AOC.Common (CharParser, pDecimal, parseMaybeLenient)
import           AOC.Common.Computer (Register(..), Memory(..), stepTilTermination, IErr(..))

import           Control.Lens hiding (op)
import           Control.Monad.Except
import           Data.Bits ((.&.), shiftL, shiftR, xor)
import           Data.Conduino
import           Data.Foldable (foldr', toList)
import           Data.List (intercalate)
import qualified Data.Conduino.Combinators as C
import qualified Data.IntMap                    as IM
import qualified Data.Map                       as M
import qualified Data.Set                       as S
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P

runProg :: Memory -> Either IErr [Int]
runProg mem = runPipe
     $ stepTilTermination mem
    .| C.sinkList

parse :: CharParser Memory
parse = do
    a <- (RegA,) <$> ("Register A: " *> pDecimal <* P.newline)
    b <- (RegB,) <$> ("Register B: " *> pDecimal <* P.newline)
    c <- (RegC,) <$> ("Register C: " *> pDecimal <* P.newline)
    prog <- (P.newline >> "Program: ") *> ((toEnum <$> pDecimal) `P.sepBy` ",")

    return $ Mem 0 (M.fromList [a,b,c]) (IM.fromList $ zip [0..] prog)

findCopier :: Memory -> [Int]
findCopier = foldr' (\o -> concatMap (`go` o)) [0] . toList . _mProg
    where
        f a = ((((a .&. 7) `xor` 2) `xor` 7) `xor` (a `shiftR` ((a .&. 7) `xor` 2))) .&. 7
        go a o = maybe [] S.toList . M.lookup o . M.fromListWith (<>) . ap (zip . fmap f) (fmap S.singleton) $ [a `shiftL` 3..(a `shiftL` 3)+7]

day17a :: Memory :~> [Int]
day17a = MkSol
    { sParse = parseMaybeLenient parse
    , sShow  = intercalate "," . map show
    , sSolve = eitherToMaybe . runProg
    }

day17b :: Memory :~> Int
day17b = MkSol
    { sParse = parseMaybeLenient parse
    , sShow  = show
    , sSolve = minimumOf traverse . findCopier
    }

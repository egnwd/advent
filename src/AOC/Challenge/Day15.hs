-- |
-- Module      : AOC.Challenge.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day15 (
    day15a
  , day15b
  ) where

import           AOC.Solver ((:~>)(..))
import AOC.Common (parseMaybeLenient, CharParser, pDecimal)

import           Data.Map                            (Map)
import           Control.Applicative                 ((<|>))
import           Data.ByteString                     (ByteString)
import           Data.Char                           (isLower)
import           Data.Foldable                       (foldl', fold)
import           Data.List                           (mapAccumL)
import           Data.List.Split                     (splitOn)
import           Data.Monoid                         (Sum(..), getSum)
import           Data.String                         (fromString)
import           Data.Word                           (Word8)
import qualified Data.ByteString                as B
import qualified Data.Map                       as M
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P

type Label = String
type Focus = Int
data Instr = Remove Label | Insert Label Focus deriving (Eq, Ord, Show)

parseInstrs :: CharParser [Instr]
parseInstrs = parseInstr `P.sepBy` (P.char ',')
    where
        parseInstr :: CharParser Instr
        parseInstr = do
            l <- P.takeWhileP Nothing isLower
            op <- (Remove <$ P.char '-') <|> (flip Insert <$> ((P.char '=') *> pDecimal))
            return $ op l

initialize :: [Instr] -> Map Word8 [(Label, Focus)]
initialize = foldl' go M.empty
    where
        go bxs (Remove l) = let h = reindeerHash (fromString l)
                             in M.alter (fmap $ filter ((/= l) .fst)) h bxs
        go bxs (Insert l f) = let h = reindeerHash (fromString l)
                               in M.insertWith placeLens h [(l, f)] bxs

        placeLens [(l,f)] lns = let (placed, lns') = mapAccumL (\p (l',f') -> if l == l' then (True, (l, f)) else (p, (l',f'))) False lns
                                 in if placed then lns' else lns ++ [(l,f)]
        placeLens _ lns = lns

focalPower (fromIntegral->bx) = fold . zipWith (\n (_,f) -> Sum (n*f*(bx+1))) [1..]

reindeerHash :: ByteString -> Word8
reindeerHash = B.foldl' go 0
    where
        go h c = (h + c) * 17

day15a :: [B.ByteString] :~> Int
day15a = MkSol
    { sParse = Just . map fromString . splitOn ","
    , sShow  = show
    , sSolve = Just . sum . map (fromIntegral . reindeerHash)
    }

day15b :: [Instr] :~> Int
day15b = MkSol
    { sParse = parseMaybeLenient parseInstrs
    , sShow  = show
    , sSolve = Just . getSum . M.foldMapWithKey focalPower . initialize
    }

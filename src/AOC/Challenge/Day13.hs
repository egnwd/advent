{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day13 (
    day13a
  , day13b
  ) where

import           AOC.Prelude

import Control.Lens

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

data Packet = Packet [Packet] | Number Int deriving (Eq)

instance Show Packet where
    show (Number n) = show n
    show (Packet ps) = show ps

instance Ord Packet where
    compare (Number a) (Number b) = compare a b
    compare (Packet a) (Packet b) = compare a b
    compare (Number a) b = compare (Packet [Number a]) b
    compare a (Number b) = compare a (Packet [Number b])

-- ^ Parsing

parsePacket, parseNumber, parseSignal :: CharParser Packet
parsePacket = P.between (P.char '[') (P.char ']') $ (Packet <$> (parseSignal `P.sepBy` P.char ','))
parseNumber = Number <$> pDecimal
parseSignal = parseNumber <|> parsePacket

parsePair :: CharParser (Packet, Packet)
parsePair = (,) <$> (parseSignal <* P.newline) <*> (parseSignal <* (optional P.newline))

correctOrder = uncurry (<)

dividers :: [Packet]
dividers =
    [ Packet [Packet [Number 2]]
    , Packet [Packet [Number 6]]
    ]

findDividers ps = traverse (\d -> succ <$> findIndex (==d) ps) dividers

day13a :: _ :~> _
day13a = MkSol
    { sParse = parseMaybeLenient (parsePair `P.sepBy` P.newline)
    , sShow  = show
    , sSolve = Just . sum . map fst . filter (correctOrder . snd) . zip [1..]
    }

day13b :: _ :~> _
day13b = MkSol
    { sParse = parseMaybeLenient (parsePair `P.sepBy` P.newline)
    , sShow  = show
    , sSolve = fmap product . findDividers . sort . (dividers ++) . concatMap (toListOf each)
    }

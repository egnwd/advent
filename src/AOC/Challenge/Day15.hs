{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day15 (
    day15a
  , day15b
  ) where

import           AOC.Prelude

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
import qualified Data.ByteString as B
import Data.String (fromString)

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

reindeerHash = B.foldl' go 0
    where
        go h c = (h + c) * 17

day15a :: [B.ByteString] :~> Int
day15a = MkSol
    { sParse = Just . map fromString . splitOn ","
    , sShow  = show
    , sSolve = Just . sum . map (fromIntegral . reindeerHash)
    }

day15b :: [Instr] :~> _
day15b = MkSol
    { sParse = parseMaybeLenient parseInstrs
    , sShow  = show
    , sSolve = Just . getSum . M.foldMapWithKey focalPower . initialize
    }

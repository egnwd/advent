{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day20
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 20.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day20 (
    day20a
  , day20b
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
import qualified Text.Megaparsec.Char.Lexer     as PP
import Control.Lens

data Module = FF { _name :: String, _state :: Bool }
            | Con { _name :: String, _inputs :: (Map String Bool) }
            | Button { _name :: String }
            | Test { _name :: String } deriving (Show)

parseModule = do
    m <- (FF <$> (P.char '%' *> pWord) <*> (pure False))
        <|> (Con <$> (P.char '&' *> pWord) <*> (pure M.empty))
        <|> (Button <$> pTok (P.string "broadcaster"))
    pTok $ P.string "->"
    sigs <- (P.takeWhileP Nothing isLower) `P.sepBy` (pTok (P.char ','))
    return (m, sigs)


build mods = M.fromList $ map go mods
    where
        go ((Con n _), rs) = (n, (Con n (M.fromSet (const False) (inputs M.! n)), rs))
        go (m, rs) = (_name m, (m, rs))
        inputs = M.fromListWith S.union [(value, S.singleton key) | (m, values) <- mods, value <- values, let key = _name m]

getSignals info n = maybe [] snd $ M.lookup n info

pressButton (pls0, info0) = go (addPulse False [1] pls0) info0 (Seq.singleton $ ("button","broadcaster", False))
    where
        go pls info (Seq.Empty) = (pls,info)
        go pls info ((from,n,p) Seq.:<| xs)
          = case M.lookup n info of
              Nothing -> go pls info xs
              Just (FF _ s, next) -> if p
                                        then go pls info xs
                                        else go (addPulse (not s) next pls) (info & ix n . _1 .~ FF n (not s)) (send xs n (not s) next)
              Just (Con _ ins, next) -> let ins' = M.insert from p ins
                                            p' = not . and $ ins'
                                         in go (addPulse p' next pls) (info & ix n . _1 .~ Con n ins') (send xs n p' next)
              Just (Button _, next) -> go (addPulse p next pls) info (send xs n p next)

send xs n p next = xs Seq.>< (Seq.fromList . map (n,,p) $ next)

addPulse False (length->n) = (+ L.V2 n 0)
addPulse True (length->n) = (+ L.V2 0 n)

day20a :: _ :~> _
day20a = MkSol
    { sParse = fmap build . parseLines parseModule
    , sShow  = show
    , sSolve = Just . product . fst . head . drop 1000 . iterate pressButton . (0,)
    }

day20b :: _ :~> _
day20b = MkSol
    { sParse = fmap build . parseLines parseModule
    , sShow  = show
    , sSolve = Just . map fst . filter (or . _inputs . fst . (M.! "qb") . snd . snd) . zip [0..] . take 100 . iterate pressButton . (0,)
    }

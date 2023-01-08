{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AOC.Challenge.Day19
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 19.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day19 (
    day19a
  , day19b
                           , createRobot
                           , Mineral(..)
                           , RobotState(..)
  ) where

import           AOC.Prelude
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.State
import           Control.Comonad.Store

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

data Mineral = Ore | Clay | Obsidian | Geode deriving (Ord, Eq, Show, Enum)

type Blueprint = Map Mineral (Map Mineral Int)

parseBlueprint :: CharParser (Int, Blueprint)
parseBlueprint = do
    id <- pTok "Blueprint" *> pDecimal <* pTok ":"
    blueprint <- M.fromList <$> P.sepEndBy parseMineralCosts (pTok ".")
    return (id, blueprint)

parseMineralCosts = do
    mineral <- pTok "Each" *> parseMineral <* pTok "robot costs"
    costs <- parseCost `P.sepBy` (pTok "and")
    return (mineral, M.fromList costs)

parseCost = do
    n <- pTok pDecimal
    m <- pTok parseMineral
    return (m, n)

parseMineral = (Ore <$ pTok "ore")
            <|> (Clay <$ pTok "clay")
            <|> (Obsidian <$ pTok "obsidian")
            <|> (Geode <$ pTok "geode")

data RobotState = RS
    { _robots :: Map Mineral Int
    , _minerals :: Map Mineral Int
    , _time :: Int
    }
    deriving (Ord, Eq, Show)

$(makeLenses ''RobotState)

initialState = RS (M.singleton Ore 1) (M.fromList $ ((,0)) <$> [Ore ..])

allWithKey p = M.foldrWithKey (\k a b -> b && p k a) True

getWithDefault :: _ -> Mineral -> RobotState -> Int
getWithDefault l m rs = fromMaybe 0 (rs ^? l . ix m)

blueprintMinerals :: Int -> Blueprint -> _
blueprintMinerals tl bp = dfs 0 start
    where
        start = initialState tl
        end (RS _ _ t) = t <= 0
        limits = M.fromListWith max (bp ^.. ((itraversed .> itraversed) . withIndex))
        potentialGeodes rs = let timeLeft = rs ^. time
                              in score rs + ((timeLeft * (timeLeft - 1)) `div` 2)
        score rs = let timeLeft = rs ^. time
                       currentGeodes = getWithDefault minerals Geode rs
                       geodeBots = getWithDefault robots Geode rs
                    in currentGeodes + (timeLeft * geodeBots)
        go :: StateT RobotState [] ()
        go = do
            (newR, rs, dt) <- lift . createRobot bp =<< get
            minerals .= rs
            rs' <- robots <%= M.insertWith (+) newR 1
            guard $ allWithKey (\k a -> a <= M.findWithDefault maxBound k limits) rs'
            time -= dt
        dfs :: Int -> RobotState -> Int
        dfs best rs
          | end rs = best
          | best >= potentialGeodes rs = best
          | otherwise = foldl' dfs (max best (score rs)) (execStateT go rs)

createRobot :: Blueprint -> RobotState -> [(Mineral, Map Mineral Int, Int)]
createRobot bp rs
    = map newRobot
    . M.toList
    . M.filterWithKey (const . valid)
    $ bp
    where
        valid Ore = True
        valid (pred->m) = getWithDefault robots m rs > 0
        newRobot (for, cost) = (for, rs', dt)
            where
                newMinerals = M.unionWith (+) (rs ^. minerals) . M.map (* dt) $ rs ^. robots
                rs' = M.differenceWith ((pure .) . (-)) newMinerals cost
                dt = succ
                   . M.foldr max 0
                   . M.mapWithKey (\m n -> let a = (n - getWithDefault minerals m rs)
                                               b = getWithDefault robots m rs
                                            in (a + b - 1) `div` b)
                   $ cost

totalQuality = getSum . foldMap (Sum . uncurry (*)) . M.toList

day19a :: Map Int Blueprint :~> _
day19a = MkSol
    { sParse = fmap M.fromList . parseLines parseBlueprint
    , sShow  = show
    , sSolve = Just . totalQuality . M.map (blueprintMinerals 24)
    }

day19b :: _ :~> _
day19b = MkSol
    { sParse = fmap M.fromList . parseLines parseBlueprint
    , sShow  = show
    , sSolve = Just . product . M.map (blueprintMinerals 32) . (`M.restrictKeys` (S.fromList [1,2,3]))
    }

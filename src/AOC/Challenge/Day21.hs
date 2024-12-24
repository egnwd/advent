{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day21
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 21.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day21 (
    day21a
  , day21b
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as Q
import           Data.Sequence                  (Seq(..))
import qualified Data.Sequence                  as Seq
import qualified Data.Sequence.NonEmpty         as NES
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP
import Data.Finite
import Control.Lens hiding (from, to)

data ControlPad = Arrow Dir | Select deriving (Eq, Ord, Generic)
instance NFData ControlPad
instance Enum ControlPad where
    toEnum 4 = Select
    toEnum n = Arrow . toEnum $ n

    fromEnum Select = 4
    fromEnum (Arrow d) = fromEnum d

instance Bounded ControlPad where
    minBound = toEnum 0
    maxBound = toEnum 4

instance Show ControlPad where
    show (Arrow North) = "^"
    show (Arrow East) = ">"
    show (Arrow South) = "v"
    show (Arrow West) = "<"
    show Select = "A"

type Actions = [ControlPad]

type NumpadDigit = Finite 11

parseNumberPad :: Char -> Maybe NumpadDigit
parseNumberPad = preview unDecDigit
numberKeypad :: Map Point NumpadDigit
numberKeypad = parseAsciiMap parseNumberPad "789\n456\n123\n 0A"

neighboursMap :: forall a. Ord a => Map Point a -> Map a (Map a (ControlPad, Int))
neighboursMap mp = M.mapWithKey (\k a -> M.fromSet (\b -> (pickAction k b, 1))
                                       . S.fromList
                                       . M.elems
                                       . (mp `M.restrictKeys`)
                                       . neighboursSet $ a) actionMap
    where
        actionMap :: Map a Point
        actionMap = M.fromList . map swap . M.toList $ mp
        pickAction :: a -> a -> ControlPad
        pickAction a b = Arrow . fromJust . vecDir $ (subtract `on` (actionMap M.!)) a b

numberPad :: Map NumpadDigit (Map NumpadDigit (ControlPad, Int))
numberPad = neighboursMap numberKeypad

parseArrowPad :: Char -> Maybe ControlPad
parseArrowPad 'A' = Just Select
parseArrowPad '^' = Just (Arrow North)
parseArrowPad '<' = Just (Arrow West)
parseArrowPad 'v' = Just (Arrow South)
parseArrowPad '>' = Just (Arrow East)
parseArrowPad _ = Nothing
arrowKeypad :: Map Point ControlPad
arrowKeypad = parseAsciiMap parseArrowPad " ^A\n<v>"

arrowPad :: Map ControlPad (Map ControlPad (ControlPad, Int))
arrowPad = neighboursMap arrowKeypad

distFromSelect :: Ord a => Map Point a -> a -> a -> Maybe Int
distFromSelect mp a x = do
    let mp' = M.fromList . map swap . M.toList $ mp
    aPos <- M.lookup a mp'
    xPos <- M.lookup x mp'
    return $ manhattan aPos xPos

newtype AStarAllState a b c = AStarAllState { _nodeQueue :: Q.OrdPSQ a (Dist c) (NES.NESeq (Set a, Seq b)) }

$(makeLenses ''AStarAllState)

initialAStarAllState :: Num c => a -> AStarAllState a b c
initialAStarAllState s = AStarAllState (Q.singleton s 0 (NES.singleton (S.singleton s, Seq.empty)))

-- | TODO: Include Heur to improve perf
allShortestPaths
  :: forall a b c. (Ord a, Ord b, Ord c, Num c, Show c, Show a, Bounded b)
  => (a -> c)              -- ^ heuristic
  -> (a -> Map a (b, c))   -- ^ neighbourhood
  -> (a -> Bool)           -- ^ termination condition
  -> a                     -- ^ start
  -> Maybe (Dist c, [Seq b]) -- ^ perhaps the cost with the path
allShortestPaths heur next end start = second (map (Seq.|> maxBound)) <$> go (initialAStarAllState start)
  where
    go :: AStarAllState a b c -> Maybe _
    go ds@(AStarAllState q0) =
      case Q.minView q0 of
        Nothing -> Nothing
        Just (n, c, p NES.:<|| ps, q)
          | end n -> Just (c, snd p : goAgain c (AStarAllState $ Q.fromList . fst . Q.atMostView c $ q'))
          | otherwise -> let ds' = ds & nodeQueue .~ q'
                             !ns = M.map (second Dist) (next n)
                          in go $ M.foldlWithKey' (updateNeighbour p c) ds' ns
          where
              q' = case NES.nonEmptySeq ps of
                     Nothing -> q
                     Just xs -> Q.insert n c xs q

    goAgain :: Dist c -> AStarAllState a b c -> [Seq b]
    goAgain minDist ds =
      case Q.minView (ds ^. nodeQueue) of
        Nothing -> []
        Just (n, c, p NES.:<|| ps, q)
          | end n -> snd p : goAgain minDist (AStarAllState $ Q.fromList . fst . Q.atMostView minDist $ q')
          | otherwise -> let ds' = ds & nodeQueue .~ q'
                             !ns = M.map (second Dist) (next n)
                          in goAgain minDist . (nodeQueue %~ Q.fromList . fst . Q.atMostView minDist) $ M.foldlWithKey' (updateNeighbour p c) ds' ns
          where
              q' = case NES.nonEmptySeq ps of
                     Nothing -> q
                     Just xs -> Q.insert n c xs q

    updateNeighbour :: (Set a, Seq b) -> Dist c -> AStarAllState a b c -> a -> (b, Dist c) -> AStarAllState a b c
    updateNeighbour (seen, pth) c ds n (act, w) =
      let cost' = w + c
          addBack = (S.insert n seen, pth Seq.|> act)
      in if n `S.member` seen
            then ds
            else case Q.lookup n (ds ^. nodeQueue) of
                   Nothing -> ds & nodeQueue %~ Q.insert n cost' (NES.singleton addBack)
                   Just (cost, pths)
                     | cost' == cost -> ds & nodeQueue %~ Q.insert n cost' (pths NES.|> addBack)
                     | cost' < cost -> ds & nodeQueue %~ Q.insert n cost' (NES.singleton addBack)
                     | otherwise -> ds


-- :( if it's optimal at the start that doesn't propogate up :(
paths :: (Bounded a, Ord a) => Map Point a -> Map Point (Map Point (Maybe (Int, [Point])))
paths mp = floydWarshall $ M.mapWithKey (\k _ -> M.mapMaybe (fmap (+1000) . distFromSelect mp maxBound) $ mp `M.restrictKeys` neighboursSet k) mp

numberInstructions :: Map NumpadDigit (Map NumpadDigit (Seq Dir))
numberInstructions = instructionSet numberKeypad

arrowInstructions :: Map ControlPad (Map ControlPad (Seq Dir))
arrowInstructions = instructionSet arrowKeypad

instructions :: Map Point (Map Point (Maybe (Int, [Point]))) -> Map Point (Map Point (Seq Dir))
instructions = M.mapWithKey (\f -> M.mapMaybe (go . (f :) . snd =<<))
    where
        go :: [Point] -> Maybe (Seq Dir)
        go = sequence . Seq.fromList . pairwise ((vecDir .) . subtract)

instructionSet :: (Ord a, Bounded a) => Map Point a -> Map a (Map a (Seq Dir))
instructionSet mp = M.mapKeys (mp M.!) . M.map (M.mapKeys (mp M.!)) . instructions . paths $ mp

goto :: (Ord a, Ord b) => a -> b -> Map a (Map b (Seq Dir)) -> Maybe (Seq Dir)
goto f t = M.lookup t <=< M.lookup f

moveArrow :: ControlPad -> ControlPad -> Maybe (Seq ControlPad)
moveArrow f t = (|> Select) . Seq.sort . fmap Arrow <$> goto f t arrowInstructions

move :: NumpadDigit -> NumpadDigit -> Maybe (Seq ControlPad)
move f t = (|> Select) . Seq.sort . fmap Arrow <$> goto f t numberInstructions

press :: NumpadDigit -> NumpadDigit -> Maybe _
press f t = do
    r1s <- toList <$> move f t
    r2s <- sequence $ pairwise moveArrow (Select : r1s)
    r3s <- sequence $ pairwise moveArrow (concatMap ((Select :) . toList) r2s)
    return $ concatMap toList r3s

solve :: [NumpadDigit] -> Maybe _
solve ns = fmap concat . sequence . pairwise press $ (10 : ns)

solve' :: [[NumpadDigit]] -> Maybe _
solve' = traverse (fmap collapse . sequence . pairwise go . (10 :))
    where
        go :: NumpadDigit -> NumpadDigit -> Maybe _
        go f t = paths numberPad f t
               >>= traverse (sequence . pairwise ((fmap (map argmin) .) . paths arrowPad) . (Select :))
               -- >>= (traverse . traverse . traverse) (sequence . pairwise ((fmap (map argmin) .) . paths arrowPad) . (Select :))

        collapse :: [[[[Maybe (ArgMin Int Actions)]]]] -> _
        collapse = robotArms1 . robotArms2 -- . robotArms3
            where
                robotArms1 :: [[[Maybe (ArgMin Int Actions)]]] -> [[Maybe (ArgMin Int Actions)]]
                robotArms1 = (fmap) fold
                robotArms2 :: [[ [[ Maybe (ArgMin Int Actions) ]] ]] -> _ -- [[Maybe (ArgMin Int Actions)]]
                robotArms2 = (fmap . fmap . fmap) (fold)
                -- robotArms3 :: [[ [[ [Maybe (ArgMin Int Actions)] ]] ]] -> _
                -- robotArms3 = (fmap . fmap . fmap) (collapse' . fmap fold)
        argmin :: Actions -> Maybe (ArgMin Int Actions)
        argmin x = Just $ Min (Arg (length x) x)
        paths :: (Ord a, Show a) => Map a (Map a (ControlPad, Int)) -> a -> a -> Maybe [Actions]
        paths pad start end = map toList . snd <$> allShortestPaths (const 1) (fromMaybe M.empty . (`M.lookup` pad)) (== end) start

collapse' :: [Maybe (ArgMin Int Actions)] -> Maybe (ArgMin Int Actions)
collapse' = fmap (fmap (foldl' addArg (Arg 0 [])) . sequence) . sequence
addArg :: Arg Int Actions -> Arg Int Actions -> Arg Int Actions
addArg (Arg n xs) (Arg m ys) = Arg (n+m) (xs ++ ys)

day21a :: [[NumpadDigit]] :~> _
day21a = MkSol
    { sParse = traverse (traverse (preview unDecDigit)) . lines
    , sShow  = show
    , sSolve = solve'
    }

day21b :: _ :~> _
day21b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }

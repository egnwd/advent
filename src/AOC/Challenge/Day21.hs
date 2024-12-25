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

module AOC.Challenge.Day21 (
    day21a
  , day21b
                           , arrowPaths
  ) where

import           AOC.Prelude

import qualified Data.Map                       as M
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Linear                         as L
import           Data.Finite
import           Control.Lens hiding (from, to)

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

type RoboticArmState = StateT (Map (Int, ControlPad, ControlPad) [Min Int]) Maybe

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
numberPad = neighboursMap $ parseAsciiMap (preview unDecDigit) "789\n456\n123\n 0A"

arrowPad :: Map ControlPad (Map ControlPad (ControlPad, Int))
arrowPad = neighboursMap $ parseAsciiMap parse " ^A\n<v>"
    where
        parse 'A' = Just Select
        parse '^' = Just (Arrow North)
        parse '<' = Just (Arrow West)
        parse 'v' = Just (Arrow South)
        parse '>' = Just (Arrow East)
        parse _ = Nothing

paths :: (Ord a) => Map a (Map a (ControlPad, Int)) -> a -> a -> Maybe [Actions]
paths pad start end = map (toList . (Seq.|> Select)) . snd
                    <$> allShortestPaths (fromMaybe mempty . (`M.lookup` pad)) (== end) start

numberPaths :: NumpadDigit -> NumpadDigit -> Maybe [Actions]
numberPaths = paths numberPad

allArrowPaths :: Map (ControlPad, ControlPad) (Maybe [Actions])
allArrowPaths = M.fromList . map (\(L.V2 a b) -> ((a,b), paths arrowPad a b)) $ sequence (return [minBound .. maxBound])

arrowPaths :: ControlPad -> ControlPad -> Maybe [Actions]
arrowPaths a b = allArrowPaths M.! (a,b)

memoize :: (Ord k, MonadState (Map k v) m) => k -> m v -> m v
memoize key compute = do
    gets (M.lookup key) >>= \case
        Just result -> return result
        Nothing -> do
          result <- compute
          modify (M.insert key result)
          return result

solve :: Int -> [[NumpadDigit]] -> Maybe _
solve r ns = do
    pths <- traverse (fmap (getMin . reduce) . pairwiseM go . (10 :)) ns
    return $ sum $ zipWith complexity pths ns
    where
        reduce :: [[Min Int]] -> Min Int
        reduce = sum . fmap fold

        go :: NumpadDigit -> NumpadDigit -> Maybe [Min Int]
        go f t = numberPaths f t >>= \nss -> evalStateT (go' r nss) M.empty

        go' :: Int -> [Actions] -> RoboticArmState [Min Int]
        go' 0 = traverse (fmap reduce . pairwiseM (\a b -> lift (arrowPaths a b) <&> map (Min . length)) . (Select :))
        go' n = traverse (fmap reduce . process . (Select :))
            where
                process :: Actions -> RoboticArmState [[Min Int]]
                process = pairwiseM $ \a b -> memoize (n, a, b) $ lift (arrowPaths a b) >>= go' (n-1)

        complexity :: Int -> [NumpadDigit] -> Int
        complexity n l = n * toNum (take 3 l)
            where
                toNum = foldl' (\acc b -> acc * 10 + (fromInteger . getFinite) b) 0


day21 :: Int -> [[NumpadDigit]] :~> Int
day21 n = MkSol
    { sParse = traverse (traverse (preview unDecDigit)) . lines
    , sShow  = show
    , sSolve = solve (n-1)
    }
day21a :: [[NumpadDigit]] :~> Int
day21a = day21 2

day21b :: [[NumpadDigit]] :~> Int
day21b = day21 25

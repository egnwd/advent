module AOC.Common.Intcode
  ( Memory(..)
  , parseMem
  , stepTilTermination
  , untilFalse
  , VMErr(..)
  , IErr(..)
  , AsVMErr(..)
  , AsIErr(..)
  ) where

import AOC.Common.Intcode.Memory
import Control.DeepSeq (NFData)
import Control.Monad.State
import Control.Monad.Except
import Control.Exception
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Traversable
import Data.Typeable
import Control.Lens.TH
import Control.Monad.Error.Lens
import GHC.Generics (Generic)
import Linear
import Text.Read (readMaybe)
import qualified Data.Map as M

-- Learning difficult Haskell via: https://github.com/mstksg/advent-of-code-2019/blob/master/src/AOC/Common/Intcode.hs

data Instr = Add | Mul | Hlt
  deriving (Eq, Ord, Enum, Show, Generic)
instance NFData Instr

instrMap :: Map Int Instr
instrMap = M.fromList $
    (99, Hlt) : zip [1 ..] [Add .. Mul]

instr :: Int -> Maybe Instr
instr = (`M.lookup` instrMap)

data VMErr = VMEBadMode  Int
           | VMEBadInstr Int
           | VMEBadPos   Int
  deriving (Eq, Ord, Show, Typeable, Generic)
instance Exception VMErr
makeClassyPrisms ''VMErr

data IErr = IENoInput
          | IEVM VMErr
  deriving (Eq, Ord, Show, Typeable, Generic)
instance Exception IErr
makeClassyPrisms ''IErr

instance AsVMErr IErr where
    _VMErr = _IEVM

parseMem :: String -> Maybe Memory
parseMem = fmap (Mem 0 . M.fromList . zip [0..])
         . traverse readMaybe
         . splitOn ","

data InstrRes = IRWrite Int         -- ^ write a value to location at
              | IRHalt              -- ^ halt
  deriving (Eq, Ord, Show, Generic)

withInput
    :: (Traversable t, Applicative t, MonadMem m)
    => (t Int -> m r)
    -> m r
withInput f = do
  let modes = pure ()
  inp <- for modes $ \_ -> mRead >>= mPeek
  f inp

intMode :: Int -> Maybe Int
intMode 0 = Just 0
intMode _ = Nothing

-- fillModes :: forall t. (Traversable t, Applicative t) => Maybe (t Int)
-- fillModes = fmap snd . traverse sequence $ mapAccumL go (0 :: Int) (pure () :: t ())
  -- where
    -- go :: Int -> _ -> (Int, Maybe Int)
    -- go a _ = (a, intMode $ a `mod` 10)

step :: (AsVMErr e, MonadError e m, MonadMem m) => m Bool
step = do
  x <- mRead
  i <- maybe (throwing _VMErr (VMEBadInstr x)) pure $ instr x
  res <- case i of
           Add -> withInput $ \(V2 a b) -> pure . IRWrite $ a + b
           Mul -> withInput $ \(V2 a b) -> pure . IRWrite $ a * b
           Hlt -> withInput $ \V0 -> pure IRHalt

  case res of
    IRWrite x -> do
      loc <- mRead
      True <$ mWrite loc x
    IRHalt -> pure False

untilFalse :: Monad m => m Bool -> m ()
untilFalse b = go
  where
    go = b >>= \case
      False -> pure ()
      True -> go

stepTilTermination :: (AsVMErr e, MonadError e m) => Memory -> m Memory
stepTilTermination m = execStateT (untilFalse step) m

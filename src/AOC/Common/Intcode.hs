module AOC.Common.Intcode
  ( Memory(..)
  , VM
  , parseMem
  , stepTilTermination
  , untilFalse
  , yieldAndDie
  , VMErr(..)
  , IErr(..)
  , AsVMErr(..)
  , AsIErr(..)
  ) where

import AOC.Common.Intcode.Memory
import AOC.Util
import Control.DeepSeq (NFData)
import Control.Exception
import Control.Lens.TH
import Control.Monad.Error.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Conduino
import Data.Conduino.Lift
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Traversable
import Data.Typeable
import Data.Void
import GHC.Generics (Generic)
import Linear
import Debug.Trace
import Text.Read (readMaybe)
import qualified Data.Map as M

-- Learning difficult Haskell via: https://github.com/mstksg/advent-of-code-2019/blob/master/src/AOC/Common/Intcode.hs

type VM = Pipe Int Int Void

data Instr = Add | Mul | Get | Put | Hlt
  deriving (Eq, Ord, Enum, Show, Generic)
instance NFData Instr

instrMap :: Map Int Instr
instrMap = M.fromList $
    (99, Hlt) : zip [1 ..] [Add .. Put]

instr :: Int -> Maybe Instr
instr = (`M.lookup` instrMap)

data Mode = Pos | Imm
  deriving (Eq, Ord, Enum, Show, Generic)
instance NFData Mode

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
              | IRNop               -- ^ no op
              | IRHalt              -- ^ halt
  deriving (Eq, Ord, Show, Generic)

withInputLazy
    :: (Traversable t, Applicative t, AsVMErr e, MonadError e m, MonadMem m)
    => Int            -- modes
    -> (t (m Int) -> m r) -- operation action
    -> m r            -- result of action
withInputLazy ms f = do
  modes <- case fillModes ms of
      Left i -> throwing _VMErr $ VMEBadMode i
      Right x -> pure x
  inp <- for modes $ \m ->
    pure $ case m of
      Pos -> mRead >>= mPeek
      Imm -> mRead
  f inp

withInput
    :: (Traversable t, Applicative t, MonadMem m, AsVMErr e, MonadError e m)
    => Int      -- ^ mode int
    -> (t Int -> m r)
    -> m r
withInput mo f = withInputLazy mo ((f =<<) . sequenceA)

intMode :: Int -> Maybe Mode
intMode = \case 0 -> Just Pos
                1 -> Just Imm
                _ -> Nothing

fillModes :: forall t. (Traversable t, Applicative t) => Int -> Either Int (t Mode)
fillModes ms = sequence . snd $ mapAccumL go ms (pure ())
  where
    go :: Int -> _ -> (Int, Either Int Mode)
    go a _ = (n, maybeToEither m $ intMode m)
      where
        (n, m) = a `divMod` 10

step :: (AsVMErr e, MonadError e m, MonadMem m) => Pipe Int Int Void m Bool
step = do
  -- Read instruction and modes 'ABCDE' where DE is the opcode and ABC are the modes
  (ms, op) <- (`divMod` 100) <$> mRead
  i <- maybe (throwing _VMErr (VMEBadInstr op)) pure $ instr op
  res <- case i of
           Add -> withInput ms $ \(V2 a b) -> pure . IRWrite $ a + b
           Mul -> withInput ms $ \(V2 a b) -> pure . IRWrite $ a * b
           Get -> withInput ms $ \V0   -> IRWrite <$> awaitSurely
           Put -> withInput ms $ \(V1 a) -> IRNop <$ yield a
           Hlt -> withInput ms $ \V0 -> pure IRHalt

  case res of
    IRWrite x -> do
      loc <- mRead
      True <$ mWrite loc x
    IRNop -> pure True
    IRHalt -> pure False

untilFalse :: Monad m => m Bool -> m ()
untilFalse b = go
  where
    go = b >>= \case
      False -> pure ()
      True -> go

stepTilTermination :: (AsVMErr e, MonadError e m) => Memory -> VM m Memory
stepTilTermination m = execStateP m (untilFalse step)

yieldAndDie :: (MonadError IErr m) => o -> Pipe i o u m a
yieldAndDie o = yield o *> throwError IENoInput

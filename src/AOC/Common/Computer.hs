module AOC.Common.Computer
  ( Register(..)
  , Memory(..)
  , VM
  , untilFalse
  , stepTilTermination
  , VMErr(..)
  , IErr(..)
  , AsVMErr(..)
  , AsIErr(..)
  ) where

import           AOC.Common.Computer.Memory
import           Control.DeepSeq           (NFData)
import           Control.Exception
import           Control.Lens              hiding (op)
import           Control.Monad.Error.Lens
import           Control.Monad.Except
import           Data.Conduino
import           Data.Conduino.Lift
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Typeable
import           Data.Void
import           GHC.Generics              (Generic)
import           Data.Bits                 ((.&.), shiftR, xor)
import           Data.Functor              (($>))

type VM = Pipe () Int Void

data Instr = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)
instance NFData Instr

data VMErr = VMEBadInstr Int
           | VMEBadOperand Int
           | VMEBadPos   Int
  deriving (Eq, Ord, Show, Typeable, Generic)
instance Exception VMErr
instance NFData VMErr
makeClassyPrisms ''VMErr

data IErr = IENoInput
          | IEVM VMErr
  deriving (Eq, Ord, Show, Typeable, Generic)
instance Exception IErr
makeClassyPrisms ''IErr

instance AsVMErr IErr where
    _VMErr = _IEVM

instance NFData IErr

data InstrRes = IRWriteReg Register Int -- ^ write a value to register
              | IRNop                   -- ^ no op
              | IRJump Int              -- ^ jump
              | IRHalt                  -- ^ halt
  deriving (Eq, Ord, Show, Generic)

instrMap :: Map Int Instr
instrMap = M.fromList $ zip [0 ..] [Adv .. ]

instr :: Int -> Maybe Instr
instr = (`M.lookup` instrMap)

literalOperand :: (AsVMErr e, MonadError e m, MonadMem m) => m Int
literalOperand = mRead >>= maybe (throwing _VMErr . VMEBadOperand =<< mCurr) pure

comboOperand :: (AsVMErr e, MonadError e m, MonadMem m) => m Int
comboOperand = do
    mRead >>= \case
        Nothing -> throwing _VMErr . VMEBadOperand =<< mCurr
        Just 4 -> mReadReg RegA
        Just 5 -> mReadReg RegB
        Just 6 -> mReadReg RegC
        Just 7 -> throwing _VMErr (VMEBadOperand 7)
        Just n -> pure n

step :: (AsVMErr e, MonadError e m, MonadMem m) => VM m Bool
step = do
  mop <- mRead
  i <- case mop of
         Nothing -> pure Nothing
         Just op -> Just <$> maybe (throwing _VMErr (VMEBadInstr op)) pure (instr op)
  res <- case i of
           Nothing -> pure IRHalt
           Just i' -> case i' of
               Adv -> do
                   a <- mReadReg RegA
                   o <- comboOperand
                   pure $ IRWriteReg RegA (a `shiftR` o)
               Bxl -> do
                   x <- xor <$> mReadReg RegB <*> literalOperand
                   pure $ IRWriteReg RegB x
               Bst -> do
                   x <- (.&.) <$> comboOperand <*> pure 7
                   pure $ IRWriteReg RegB x
               Jnz -> do
                   a <- mReadReg RegA
                   if a == 0 then mRead $> IRNop else IRJump <$> literalOperand
               Bxc -> do
                   _ <- mRead
                   x <- xor <$> mReadReg RegB <*> mReadReg RegC
                   pure $ IRWriteReg RegB x
               Out -> do
                   x <- (.&.) <$> comboOperand <*> pure 7
                   IRNop <$ yield x
               Bdv -> do
                   a <- mReadReg RegA
                   o <- comboOperand
                   pure $ IRWriteReg RegB (a `shiftR` o)
               Cdv -> do
                   a <- mReadReg RegA
                   o <- comboOperand
                   pure $ IRWriteReg RegC (a `shiftR` o)

  case res of
    IRWriteReg r x -> do
        True <$ mWriteReg r x
    IRJump j -> True <$ mSeek j
    IRNop -> pure True
    IRHalt -> pure False

untilFalse :: Monad m => m Bool -> m ()
untilFalse b = go
  where
    go = b >>= \case
      False -> pure ()
      True -> go

stepTilTermination :: (AsVMErr e, MonadError e m) => Memory -> VM m Memory
stepTilTermination mem = execStateP mem (untilFalse step)

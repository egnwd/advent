{-# LANGUAGE OverloadedStrings #-}
module AOC.Common.Intcode
  ( Memory(..)
  , VM
  , parseMem
  , stepTilTermination
  , untilFalse
  , untilHalt
  , yieldAndDie
  , yieldAndPass
  , VMErr(..)
  , IErr(..)
  , AsVMErr(..)
  , AsIErr(..)
  ) where

import           AOC.Common.Intcode.Memory
import           AOC.Util
import           Control.DeepSeq           (NFData)
import           Control.Exception
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.Error.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Conduino
import qualified Data.Conduino.Combinators as C
import           Data.Conduino.Lift
import           Data.List.Split           (splitOn)
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Traversable
import           Data.Typeable
import           Data.Void
import           GHC.Generics              (Generic)
import           Linear
import           Text.Read                 (readMaybe)

-- Learning difficult Haskell via: https://github.com/mstksg/advent-of-code-2019/blob/master/src/AOC/Common/Intcode.hs

type VM = Pipe Int Int Void

data Instr = Add | Mul | Get | Put | Jnz | Jez | Clt | Ceq | Hlt
  deriving (Eq, Ord, Enum, Show, Generic)
instance NFData Instr

instrMap :: Map Int Instr
instrMap = M.fromList $
    (99, Hlt) : zip [1 ..] [Add .. Ceq]

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
              | IRJump Int          -- ^ jump
              | IRHalt              -- ^ halt
  deriving (Eq, Ord, Show, Generic)

withInputLazy
    :: (Traversable t, Applicative t, AsVMErr e, MonadError e m, MonadMem m)
    => Int                -- ^ modes
    -> (t (m Int) -> m r) -- ^ operation action
    -> m (r, Mode)        -- ^ result of action
withInputLazy ms f = do
  (lastMode, modes) <- case fillModes ms of
      Left i  -> throwing _VMErr $ VMEBadMode i
      Right x -> pure x
  inp <- for modes $ \m ->
    pure $ case m of
      Pos -> mRead >>= mPeek
      Imm -> mRead
  (, lastMode) <$> f inp

withInput
    :: (Traversable t, Applicative t, MonadMem m, AsVMErr e, MonadError e m)
    => Int            -- ^ mode int
    -> (t Int -> m r)
    -> m (r, Mode)
withInput mo f = withInputLazy mo ((f =<<) . sequenceA)

intMode :: Int -> Maybe Mode
intMode = \case 0 -> Just Pos
                1 -> Just Imm
                _ -> Nothing

fillModes :: forall t. (Traversable t, Applicative t) => Int -> Either Int (Mode, t Mode)
fillModes ms = traverse sequence (mapAccumL go ms (pure ())) >>= traverseOf _1 parseMode
  where
    parseMode = maybeToEither <*> intMode
    go :: Int -> _ -> (Int, Either Int Mode)
    go a _ = second parseMode $ a `divMod` 10

step :: (AsVMErr e, MonadError e m, MonadMem m) => Pipe Int Int Void m Bool
step = do
  -- Read instruction and modes 'ABCDE' where DE is the opcode and ABC are the modes
  (ms, op) <- (`divMod` 100) <$> mRead
  i <- maybe (throwing _VMErr (VMEBadInstr op)) pure $ instr op
  (res, lastMode) <- case i of
           Add -> withInput ms $ \(V2 a b) -> pure . IRWrite $ a + b
           Mul -> withInput ms $ \(V2 a b) -> pure . IRWrite $ a * b
           Get -> withInput ms $ \V0   -> IRWrite <$> awaitSurely
           Put -> withInput ms $ \(V1 a) -> IRNop <$ yield a
           Jnz -> withInput ms $ \(V2 a l) -> pure $ if a /= 0 then IRJump l else IRNop
           Jez -> withInput ms $ \(V2 a l) -> pure $ if a == 0 then IRJump l else IRNop
           Clt -> withInput ms $ \(V2 a b) -> pure . IRWrite $ if a < b then 1 else 0
           Ceq -> withInput ms $ \(V2 a b) -> pure . IRWrite $ if a == b then 1 else 0
           Hlt -> withInput ms $ \V0 -> pure IRHalt

  case res of
    IRWrite x -> case lastMode of
      Pos -> do
        loc <- mRead
        True <$ mWrite loc x
      Imm -> do
        _ <- mRead
        loc <- mCurr
        True <$ mWrite loc x
    IRJump j -> True <$ mSeek j
    IRNop -> pure True
    IRHalt -> pure False

untilHalt
    :: Monad m
    => Pipe i o u (ExceptT e m) a
    -> Pipe i o u m             ()
untilHalt = runExceptP_

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

yieldAndPass :: o -> Pipe o o u m u
yieldAndPass i = yield i *> C.map id

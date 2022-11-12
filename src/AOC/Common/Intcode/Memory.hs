module AOC.Common.Intcode.Memory
  ( Memory(..)
  , MonadMem(..)
  , _mPos
  , _mRegs
  ) where

import           Control.DeepSeq     (NFData)
import           Control.Lens
import           Control.Monad.State
import           Data.Conduino
import           Data.Conduino.Lift
import           Data.Map            (Map)
import qualified Data.Map            as M
import           GHC.Generics        (Generic)

data Memory = Mem
  { mPos  :: Int
  , mBase :: Int
  , mRegs :: Map Int Int
  }
  deriving (Eq, Ord, Show, Generic)
instance NFData Memory

_mPos :: Lens' Memory Int
_mPos = lens mPos (\mem newPos -> mem { mPos = newPos })

_mBase :: Lens' Memory Int
_mBase = lens mBase (\mem newBase -> mem { mBase = newBase })

_mRegs :: Lens' Memory (Map Int Int)
_mRegs = lens mRegs (\mem newRegs -> mem { mRegs = newRegs })

class Monad m => MonadMem m where
    mRead      :: m Int
    mCurr      :: m Int
    mPeek      :: Int -> m Int
    mSeek      :: Int -> m ()
    mWrite     :: Int -> Int -> m ()
    mShiftBase :: Int -> m ()
    mWithBase  :: Int -> m Int

instance Monad m => MonadMem (StateT Memory m) where
    mRead = do
      Mem{..} <- get
      M.findWithDefault 0 mPos mRegs <$ (_mPos += 1)
    mCurr = gets mPos
    mPeek i = gets $ M.findWithDefault 0 i . mRegs
    mSeek = assign _mPos
    mWrite i x = _mRegs %= M.insert i x
    mShiftBase i = _mBase += i
    mWithBase i = (+i) <$> gets mBase

instance MonadMem m => MonadMem (Pipe i o u m) where
    mRead = lift mRead
    mCurr = lift mCurr
    mPeek = lift . mPeek
    mSeek = lift . mSeek
    mWrite i = lift . mWrite i
    mShiftBase = lift . mShiftBase
    mWithBase = lift . mWithBase

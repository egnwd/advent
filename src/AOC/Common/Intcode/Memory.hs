module AOC.Common.Intcode.Memory
  ( Memory(..)
  , MonadMem(..)
  , _mPos
  , _mRegs
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Map (Map)
import Control.Lens
import           Control.Monad.State
import qualified Data.Map as M

data Memory = Mem
  { mPos :: Int
  , mRegs :: Map Int Int
  }
  deriving (Eq, Ord, Show, Generic)
instance NFData Memory

_mPos :: Lens' Memory Int
_mPos = lens mPos (\mem newPos -> mem { mPos = newPos })

_mRegs :: Lens' Memory (Map Int Int)
_mRegs = lens mRegs (\mem newRegs -> mem { mRegs = newRegs })

class Monad m => MonadMem m where
    mRead      :: m Int
    mCurr      :: m Int
    mPeek      :: Int -> m Int
    mSeek      :: Int -> m ()
    mWrite     :: Int -> Int -> m ()

instance Monad m => MonadMem (StateT Memory m) where
    mRead = do
      Mem{..} <- get
      M.findWithDefault 0 mPos mRegs <$ (_mPos += 1)
    mCurr = gets mPos
    mPeek i = gets $ M.findWithDefault 0 i . mRegs
    mSeek = assign _mPos
    mWrite i x = _mRegs %= M.insert i x

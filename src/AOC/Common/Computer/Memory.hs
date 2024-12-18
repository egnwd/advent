module AOC.Common.Computer.Memory
  ( Register(..)
  , Memory(..)
  , MonadMem(..)
  , mPos
  , mRegs
  ) where

import           Control.DeepSeq     (NFData)
import           Control.Lens
import           Control.Monad.State
import           Data.Conduino
import           Data.Map            (Map)
import           Data.IntMap         (IntMap)
import qualified Data.Map            as M
import qualified Data.IntMap         as IM
import           GHC.Generics        (Generic)

data Register = RegA | RegB | RegC deriving (Eq, Ord, Show, Generic)
instance NFData Register

data Memory = Mem
    { _mPos :: Int
    , _mRegs :: Map Register Int
    , _mProg :: IntMap Int
    } deriving (Eq, Ord, Show, Generic)
instance NFData Memory
makeLenses ''Memory

class Monad m => MonadMem m where
    mRead      :: m (Maybe Int)
    mReadReg   :: Register -> m Int
    mCurr      :: m Int
    mPeek      :: Int -> m Int
    mSeek      :: Int -> m ()
    mWrite     :: Int -> Int -> m ()
    mWriteReg  :: Register -> Int -> m ()

instance Monad m => MonadMem (StateT Memory m) where
    mRead = do
      Mem{..} <- get
      IM.lookup _mPos _mProg <$ (mPos += 1)
    mReadReg r = do
      Mem{..} <- get
      return $ M.findWithDefault 0 r _mRegs
    mCurr = gets _mPos
    mPeek i = gets $ IM.findWithDefault 0 i . _mProg
    mSeek = assign mPos
    mWrite i x = mProg %= IM.insert i x
    mWriteReg i x = mRegs %= M.insert i x

instance MonadMem m => MonadMem (Pipe i o u m) where
    mRead    = lift mRead
    mReadReg = lift . mReadReg
    mCurr    = lift mCurr
    mPeek    = lift . mPeek
    mSeek    = lift . mSeek
    mWrite i = lift . mWrite i
    mWriteReg i = lift . mWriteReg i


module Shuffle.Random where

import Control.Monad.State (State, StateT)
import qualified Control.Monad.State as State
import Control.Monad.Trans (MonadTrans)
import System.Random (Random, StdGen)
import qualified System.Random as Random

newtype RandomState a = RandomState {runRandomState :: State StdGen a}
  deriving newtype (Functor, Applicative, Monad)

newtype RandomStateT m a = RandomStateT {runRandomStateT :: StateT StdGen m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

-- random :: RandomGen g => g -> (a, g)
rand :: Random a => RandomState a
rand = RandomState do
  (x, g) <- State.gets Random.random
  State.put g
  pure x

rand' :: (Monad m, Random a) => RandomStateT m a
rand' = RandomStateT do
  (x, g) <- State.gets Random.random
  State.put g
  pure x

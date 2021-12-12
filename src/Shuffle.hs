module Shuffle where

import qualified Card
import Card (Card)
import qualified Shuffle.Random as Random
import Shuffle.Random (RandomState)
import System.Random (Random, RandomGen)

class Monad m => MonadRandomState m where
  randomS :: forall a. (Random a) => m (RandomState a)
-- shuffle :: MonadRandomState m => [Card] -> m [Card]
-- shuffle cards = do
--   rand <- randomS
--   rand

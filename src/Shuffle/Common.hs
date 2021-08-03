{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shuffle.Common where

import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.IO.Class as IO.Class
import Control.Monad.State (State, StateT)
import qualified Control.Monad.State as State
import Control.Monad.Trans (MonadTrans)
import qualified Data.List as List
import System.Random (Random, RandomGen, StdGen)
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

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle gen = go gen []
  where
    go gen' acc [] = acc
    go gen' acc cards =
      let (n, gen'') = Random.next gen'
          (cards', pick : cards'') = List.splitAt (n `mod` length cards) cards
       in go gen'' (pick : acc) (cards' ++ cards'')

shuffle' :: StdGen -> [a] -> [a]
shuffle' g = flip State.evalState g . runRandomState . go []
  where
    go :: [a] -> [a] -> RandomState [a]
    go acc [] = pure acc
    go acc cards = do
      n <- rand
      let (cards', pick : cards'') = List.splitAt (n `mod` length cards) cards
      go (pick : acc) (cards' ++ cards'')

shuffle'' :: forall m a. Monad m => StdGen -> [a] -> m [a]
shuffle'' g = flip State.evalStateT g . runRandomStateT . go []
  where
    go :: [a] -> [a] -> RandomStateT m [a]
    go acc [] = pure acc
    go acc cards = do
      n <- rand'
      let (cards', pick : cards'') = List.splitAt (n `mod` length cards) cards
      go (pick : acc) (cards' ++ cards'')

shuffle''' :: MonadIO m => [a] -> m [a]
shuffle''' cards = do
  g <- IO.Class.liftIO Random.newStdGen
  shuffle'' g cards

-- TODO: Shuffle w
-- - DList
-- - STArray
-- - (Mutable)Vector (works in ST)

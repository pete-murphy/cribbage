{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shuffle.List where

import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.IO.Class as IO.Class
import qualified Control.Monad.State as State
import qualified Data.List as List
import Shuffle.Random as Random
import System.Random (RandomGen, StdGen)
import qualified System.Random

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle gen xs = go gen [] xs (length xs)
  where
    go _ acc _ 0 = acc
    go gen' acc xs l =
      let (n, gen'') = System.Random.next gen'
          (xs', pick : xs'') = List.splitAt (n `mod` l) xs
       in go gen'' (pick : acc) (xs' ++ xs'') (l - 1)

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
  g <- IO.Class.liftIO System.Random.newStdGen
  shuffle'' g cards
-- TODO: Shuffle w
-- - DList
-- - STArray
-- - (Mutable)Vector (works in ST)

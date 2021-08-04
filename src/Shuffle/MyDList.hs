{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shuffle.MyDList where

import System.Random (RandomGen)

newtype DList a = DL {unDL :: [a] -> [a]}

empty :: DList a
empty = DL \_ -> []

-- *******************************

shuffle :: RandomGen g => g -> DList a -> DList a
shuffle gen = go gen empty
  where
    go = undefined
-- go _ acc [] = acc
-- go gen' acc cards =
--   let (n, gen'') = System.Random.next gen'
--       (cards', pick : cards'') = List.splitAt (n `mod` length cards) cards
--    in go gen'' (pick : acc) (cards' ++ cards'')
-- shuffle' :: StdGen -> DList a -> DList a
-- shuffle' g = flip State.evalState g . Random.runRandomState . go []
--   where
--     go :: [a] -> [a] -> RandomState [a]
--     go acc [] = pure acc
--     go acc cards = do
--       n <- Random.rand
--       let (cards', pick : cards'') = List.splitAt (n `mod` length cards) cards
--       go (pick : acc) (cards' ++ cards'')
-- shuffle'' :: forall m a. Monad m => StdGen -> DList a -> m (DList a)
-- shuffle'' g = flip State.evalStateT g . Random.runRandomStateT . go []
--   where
--     go :: DList a -> DList a -> RandomStateT m (DList a)
--     go acc [] = pure acc
--     go acc cards = do
--       n <- Random.rand'
--       let (cards', pick : cards'') = List.splitAt (n `mod` length cards) cards
--       go (pick : acc) (cards' ++ cards'')

-- shuffle''' :: MonadIO m => DList a -> m (DList a)
-- shuffle''' cards = do
--   g <- IO.Class.liftIO System.Random.newStdGen
--   shuffle'' g cards

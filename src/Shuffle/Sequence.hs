{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Shuffle.Sequence where

import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.IO.Class as IO.Class
import qualified Control.Monad.State as State
import Data.Foldable
import qualified Data.Sequence as Sequence
import Data.Sequence (Seq)
import Shuffle.Random as Random
import System.Random (RandomGen, StdGen)
import qualified System.Random

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle gen (Sequence.fromList -> s) = toList (go gen Sequence.empty s)
  where
    go :: RandomGen g => g -> Seq a -> Seq a -> Seq a
    go = undefined
-- go _ acc [] = acc
-- go gen' acc xs =
--   let (n, gen'') = System.Random.next gen'
--       (xs', pick : xs'') = List.splitAt (n `mod` length xs) xs
--    in go gen'' (pick : acc) (xs' <> xs'')

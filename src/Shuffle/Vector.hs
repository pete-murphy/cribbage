{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Shuffle.Vector where

import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.IO.Class as IO.Class
import qualified Control.Monad.ST as ST
import Control.Monad.ST (ST)
import qualified Control.Monad.State as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.STRef as STRef
import qualified Data.STRef (STRef)
import qualified Data.Vector as Vector
import Data.Vector (MVector, Vector)
import qualified Data.Vector.Mutable as Vector.Mutable
import System.Random (RandomGen, StdGen)
import qualified System.Random as Random

shuffle :: forall g a. RandomGen g => g -> [a] -> [a]
shuffle g (Vector.fromList -> v) = Vector.toList newVector
  where
    newVector =
      Vector.create do
        let len = Vector.length v
        ref <- STRef.newSTRef g
        v' <- Vector.thaw v
        Foldable.for_ [0 .. len - 1] \i -> do
          g' <- STRef.readSTRef ref
          let ((`mod` len) -> j, g'') = Random.random g'
          Vector.Mutable.swap v' i j
          STRef.writeSTRef ref g''
        pure v'

something :: RandomGen g => g -> ST s (ST s Int)
something g = do
  ref <- STRef.newSTRef g
  pure do
    g' <- STRef.readSTRef ref
    let (n, g'') = Random.random g'
    STRef.writeSTRef ref g''
    pure n

shuffle' :: forall g a. RandomGen g => g -> [a] -> [a]
shuffle' g xs = Vector.toList newVector
  where
    newVector = Vector.create do
      let len = length xs
      v <- Vector.Mutable.new len
      Foldable.for_ (zip [0 ..] xs) \(i, x) -> Vector.Mutable.write v i x
      x <- something g
      Foldable.for_ [0 .. len - 1] \i -> do
        j <- (`mod` len) <$> x
        Vector.Mutable.swap v i j
      pure v

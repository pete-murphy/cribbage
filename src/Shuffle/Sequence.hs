{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Shuffle.Sequence where

import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.IO.Class as IO.Class
import qualified Control.Monad.State as State
import Data.Foldable
import qualified Data.Sequence as Sequence
import Data.Sequence ((><), Seq, ViewL (..))
import Shuffle.Random as Random
import System.Random (RandomGen, StdGen)
import qualified System.Random

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle gen (Sequence.fromList -> s) = go gen [] s (length s)
  where
    -- go :: RandomGen g => g -> Seq a -> Seq a -> Seq a
    go _ acc _ 0 = acc
    go g acc s l =
      let (n, g') = System.Random.next g
          (leftS, (Sequence.viewl -> (pick :< rightS))) = Sequence.splitAt (n `mod` l) s
       in go g' (pick : acc) (leftS >< rightS) (l - 1)

-- Same shuffle algo as Vector thing
shuffle' :: RandomGen g => g -> [a] -> [a]
shuffle' gen (Sequence.fromList -> s) = toList s'
  where
    ijs = map (`mod` length s) (System.Random.randoms gen) `zip` [0 .. length s - 1]
    s' = foldl' (flip ($)) s (uncurry swap <$> ijs)

swap :: Int -> Int -> Seq a -> Seq a
swap i j s =
  let (x, y) = (s `Sequence.index` i, s `Sequence.index` j)
   in Sequence.update i y (Sequence.update j x s)

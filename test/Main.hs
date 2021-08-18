{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Foldable
import qualified Data.Set as Set
import qualified Shuffle.List as List
import qualified Shuffle.Sequence as Sequence
import qualified Shuffle.Vector as Vector
import qualified System.Random as Random
import System.Random (RandomGen, StdGen)
import Test.QuickCheck
import Test.QuickCheck.Property (Prop)

type Shuffle g a = g -> [a] -> [a]

prop_lengthPreserving :: Shuffle StdGen Int -> Int -> [Int] -> Bool
prop_lengthPreserving shuffle n xs =
  length (shuffle (Random.mkStdGen n) xs) == length xs

prop_sameElements :: Shuffle StdGen Int -> Int -> [Int] -> Bool
prop_sameElements shuffle n xs =
  Set.fromList (shuffle (Random.mkStdGen n) xs) == Set.fromList xs

prop_sameElements' :: Shuffle StdGen Int -> Int -> [Int] -> Bool
prop_sameElements' shuffle n xs =
  Set.fromList (shuffle (Random.mkStdGen n) xs) == Set.fromList xs

main = do
  let properties = [prop_lengthPreserving, prop_sameElements, prop_sameElements']
      shuffles = [List.shuffle', Vector.shuffle, Vector.shuffle', Sequence.shuffle']
      args = ($) <$> properties <*> shuffles
  traverse_ quickCheck' args
  where
    quickCheck' = quickCheckWith (stdArgs {maxSuccess = 1000})

module Main where

import qualified Data.Foldable as Foldable
import qualified Data.Set as Set
import qualified Shuffle.List as List
import qualified Shuffle.Sequence as Sequence
import qualified Shuffle.Vector as Vector
import qualified System.Random as Random
import System.Random (RandomGen, StdGen)
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck as QuickCheck (Args (..))
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

main :: IO ()
main = do
  let properties = [prop_lengthPreserving, prop_sameElements, prop_sameElements']
      shuffles = [List.shuffle', Vector.shuffle, Vector.shuffle', Sequence.shuffle']
      args = ($) <$> properties <*> shuffles
  Foldable.traverse_ quickCheck' args
  where
    quickCheck' = QuickCheck.quickCheckWith (QuickCheck.stdArgs {maxSuccess = 1000})

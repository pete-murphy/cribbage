module Main where

import qualified Shuffle.List as List
import qualified System.Random as Random
import Test.QuickCheck (Gen, Testable, quickCheck)
import Test.QuickCheck.Property (Prop)

prop_commutativeAdd :: Int -> Int -> Bool
prop_commutativeAdd x y = x + y == y + x

-- prop_lengthPreserving :: Testable prop => prop
prop_lengthPreserving :: Int -> [Int] -> Bool
prop_lengthPreserving n xs =
  length (List.shuffle (Random.mkStdGen n) xs) == if n < 20 then length xs else 20

main = do
  quickCheck prop_lengthPreserving

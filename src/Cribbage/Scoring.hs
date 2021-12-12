module Cribbage.Scoring where

import Card (Card (..), Rank (..), Suit (..))
import qualified Card
import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.Function as Function
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Maybe as Maybe
import Data.Monoid (Sum (Sum))
import qualified Data.Monoid as Monoid
import qualified Data.Ord as Ord

scoreHand :: [Card] -> Int
scoreHand [] = 0
scoreHand cs = countPairs cs * 2 + scoreRuns cs + scoreFifteens cs

longestCommonPrefix :: Eq a => [a] -> [a] -> [a]
longestCommonPrefix (x : xs) (y : ys) =
  if x == y then x : longestCommonPrefix xs ys else []
longestCommonPrefix _ _ = []

scoreFifteens :: [Card] -> Int
scoreFifteens cs = sum do
  cs' <- List.subsequences cs
  Monad.guard (Foldable.foldMap (Sum . Card.value) cs' == 15)
  pure 2

scoreRuns :: [Card] -> Int
scoreRuns cards =
  let histogram = Map.fromListWith (+) [(r, 1) | r `Of` _ <- cards]
      keys = Map.keys histogram
      ranks = [Ace .. King]
      runs = longestCommonPrefix <$> List.tails keys <*> List.tails ranks
      longestRun = List.maximumBy (Ord.comparing length) runs
      n = product (flip Map.lookup histogram `Maybe.mapMaybe` longestRun)
   in n * length longestRun

countPairs :: [Card] -> Int
countPairs =
  Monoid.getSum
    . Foldable.foldMap (Sum . (`choose` 2) . length)
    . List.groupBy ((==) `Function.on` rank)
    . List.sortOn rank -- effectively same as `sort`

choose :: Int -> Int -> Int
choose n r = fact n `div` (fact r * fact (n - r))

fact :: Int -> Int
fact n = product [1 .. n]

-- | Example hands

-- | Score would be 16 for this hand
cards :: [Card]
cards =
  [ Ace `Of` Hearts,
    Two `Of` Hearts,
    Two `Of` Diamonds,
    Three `Of` Diamonds,
    Three `Of` Clubs
  ]

-- | Score would be 29 for this hand
cards' :: [Card]
cards' =
  [ Five `Of` Hearts,
    Five `Of` Clubs,
    Five `Of` Diamonds,
    Five `Of` Spades,
    Ten `Of` Clubs
  ]

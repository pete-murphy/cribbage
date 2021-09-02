module Cribbage.Scoring where

import Card
import Data.Function (on)
import qualified Data.Graph as Graph
import qualified Data.List as List

scoreHand :: [Card] -> Int
scoreHand [] = 0
scoreHand cs = countPairs cs * 2 + countRuns cs * 2

countRuns :: [Card] -> Int
countRuns cards =
  let edges = toEdges =<< List.tails cards
      toEdges [] = []
      toEdges (n : ns) = [(n, n, next)]
        where
          next =
            takeWhile
              -- TODO: succ is unsafe
              ((== succ (rank n)) . rank)
              (dropWhile ((== rank n) . rank) ns)
      (graph, fromVertex, _) = Graph.graphFromEdges edges
   in length (Graph.edges graph)

countPairs :: [Card] -> Int
countPairs =
  sum
    . map ((`choose` 2) . length)
    . List.groupBy ((==) `on` rank)
    . List.sortOn rank -- effectively same as `sort`

choose :: Int -> Int -> Int
choose n r = fact n `div` (fact r * fact (n - r))

fact :: Int -> Int
fact n = product [1 .. n]

cards :: [Card]
cards =
  [ Ace `Of` Hearts,
    Two `Of` Hearts,
    Two `Of` Diamonds,
    Three `Of` Diamonds,
    Three `Of` Clubs
  ]

-- This works
xs = [1, 2, 3]

xl = length xs

avg = (/) (sum xs) (fromIntegral (xl))

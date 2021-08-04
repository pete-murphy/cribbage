module Benchmark where

import Criterion.Main
import qualified Shuffle.List as List
import qualified Shuffle.Vector as Vector
import System.Random

main :: IO ()
main =
  let stdGen = mkStdGen 42
   in defaultMain
        [ bgroup
            "List.shuffle"
            [ bench "1" $ whnf (List.shuffle stdGen) [0 .. 10000]
            ]
        ]

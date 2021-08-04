module Main where

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
            [ bench "10" $ whnf (List.shuffle stdGen) [0 .. 10],
              bench "1000" $ whnf (List.shuffle stdGen) [0 .. 1000],
              bench "10000" $ whnf (List.shuffle stdGen) [0 .. 10000]
            ],
          bgroup
            "Vector.shuffle"
            [ bench "10" $ whnf (Vector.shuffle stdGen) [0 .. 10],
              bench "1000" $ whnf (Vector.shuffle stdGen) [0 .. 1000],
              bench "10000" $ whnf (Vector.shuffle stdGen) [0 .. 10000]
            ],
          bgroup
            "Vector.shuffle'"
            [ bench "10" $ whnf (Vector.shuffle' stdGen) [0 .. 10],
              bench "1000" $ whnf (Vector.shuffle' stdGen) [0 .. 1000],
              bench "10000" $ whnf (Vector.shuffle' stdGen) [0 .. 10000]
            ]
        ]

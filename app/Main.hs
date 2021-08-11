module Main where

import Criterion.Main
import qualified Shuffle.List as List
import qualified Shuffle.Sequence as Sequence
import qualified Shuffle.Vector as Vector
import System.Random

main :: IO ()
main =
  let stdGen = mkStdGen 42
   in defaultMain
        [ bgroup
            "List.shuffle"
            [ -- bench "10" $ whnf (List.shuffle stdGen) [0 .. 10],
              -- bench "1000" $ whnf (List.shuffle stdGen) [0 .. 1000],
              -- bench "50000" $ whnf (List.shuffle stdGen) [0 .. 50000],
              -- bench "100000" $ whnf (List.shuffle stdGen) [0 .. 100000],
              -- bench "500000" $ whnf (List.shuffle stdGen) [0 .. 500000],
              bench "5,000,000" $ whnf (List.shuffle stdGen) [0 .. 5_000_000]
            ],
          -- bgroup
          --   "Vector.shuffle"
          --   [ bench "10" $ whnf (Vector.shuffle stdGen) [0 .. 10],
          --     bench "1000" $ whnf (Vector.shuffle stdGen) [0 .. 1000],
          --     bench "50000" $ whnf (Vector.shuffle stdGen) [0 .. 50000],
          --     bench "100000" $ whnf (Vector.shuffle stdGen) [0 .. 100000]
          --   ],
          -- -- bgroup
          -- --   "Vector.shuffle'"
          -- --   [ bench "10" $ whnf (Vector.shuffle' stdGen) [0 .. 10],
          -- --     bench "1000" $ whnf (Vector.shuffle' stdGen) [0 .. 1000],
          -- --     bench "50000" $ whnf (Vector.shuffle' stdGen) [0 .. 50000],
          -- --     bench "100000" $ whnf (Vector.shuffle' stdGen) [0 .. 100000]
          -- --   ],
          bgroup
            "Sequence.shuffle"
            [ -- bench "10" $ whnf (Sequence.shuffle stdGen) [0 .. 10],
              -- bench "1000" $ whnf (Sequence.shuffle stdGen) [0 .. 1000],
              -- bench "50000" $ whnf (Sequence.shuffle stdGen) [0 .. 50000],
              -- bench "100000" $ whnf (Sequence.shuffle stdGen) [0 .. 100000],
              -- bench "500000" $ whnf (Sequence.shuffle stdGen) [0 .. 500000],
              bench "5,000,000" $ whnf (Sequence.shuffle stdGen) [0 .. 5_000_000]
            ]
          -- bgroup
          --   "Sequence.shuffle'"
          --   [ bench "10" $ whnf (Sequence.shuffle' stdGen) [0 .. 10],
          --     bench "1000" $ whnf (Sequence.shuffle' stdGen) [0 .. 1000],
          --     bench "50000" $ whnf (Sequence.shuffle' stdGen) [0 .. 50000],
          --     bench "100000" $ whnf (Sequence.shuffle' stdGen) [0 .. 100000]
          --   ]
        ]

module Main where

import qualified Cribbage.Scoring as Scoring
import qualified Data.Foldable as Foldable
import qualified Data.Graph as Graph

main :: IO ()
main = do
  pure ()
-- let g = Scoring.cardsGraph Scoring.cards
-- Foldable.traverse_
--   (putStrLn . Scoring.drawTree)
--   -- (Graph.dfs g [0 .. 4])
--   (Graph.dff g)

module Main where

import qualified Cribbage.Game as Game
import qualified Cribbage.GameState as GameState

main :: IO ()
main = do
  winner <- GameState.runGame Game.game
  putStrLn ("Winner is: " <> show winner)
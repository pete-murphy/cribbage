module Main where

import qualified Card
import Control.Lens.Combinators
import Control.Lens.Operators
import qualified Cribbage.Game as Game
import Cribbage.Game (Player (..))
import qualified Cribbage.Scoring as Scoring
import qualified Data.Foldable as Foldable
import qualified Data.Graph as Graph
import qualified System.Random as Random

main :: IO ()
main = do
  test 0

-- Temporary debugging
test :: Int -> IO ()
test n = do
  -- gen <- Random.getStdGen
  let gen = Random.mkStdGen n
      gs = Game.initialState gen
      afterDeal = Game.deal 6 gs
  putStrLn "Deck after deal:"
  Foldable.traverse_ (putStr . (<> " ") . Card.display) (Game.deck afterDeal)
  ---
  lineBreak
  debug "A hand before deal" (gs ^. #play . #playerHands . ix A)
  debug "B hand before deal" (gs ^. #play . #playerHands . ix B)
  debug "A hand after deal" (afterDeal ^. #play . #playerHands . ix A)
  debug "B hand after deal" (afterDeal ^. #play . #playerHands . ix B)
  ---
  lineBreak
  let cribA = afterDeal ^.. #play . #playerHands . ix A . taking 2 folded
      cribB = afterDeal ^.. #play . #playerHands . ix B . taking 2 folded
      afterCrib = Game.makeCrib cribA cribB afterDeal
  debug "cribA" cribA
  debug "cribB" cribB
  ---
  lineBreak
  debug "A hand after crib" (afterCrib ^. #play . #playerHands . ix A)
  debug "B hand after crib" (afterCrib ^. #play . #playerHands . ix B)
  ---
  lineBreak
  Foldable.traverse_ (debug "final crib") (Game.crib (Game.play afterCrib))
  where
    debug msg a = putStrLn (msg <> ":\n" <> (Card.display =<< a))
    lineBreak = putStrLn "\n"

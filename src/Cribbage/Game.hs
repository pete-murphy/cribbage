module Cribbage.Game where

import Card (Card)
import qualified Card
import Data.Map (Map)
import qualified Data.Map as Map

-- Fixing the number of players to two for now
data Player = A | B
  deriving (Eq, Ord, Show)

newtype Board
  = Board (Map Player Int)

data PlayState
  = Initial
      (Map Player [Card]) -- player hands
  | Crib
      (Map Player [Card]) -- player hands
      [Card] -- crib
  | CutCard
      (Map Player [Card]) -- player hands
      [Card] -- crib
      Card -- cut card
  | Play
      (Map Player [Card]) -- player hands
      [Card] -- crib
      Card -- cut card
      [Card] -- cards on table
      Player -- whose turn is it?

data GameState
  = GameState
      { dealer :: Player,
        deck :: [Card],
        board :: Board,
        play :: PlayState
      }

-- TODO: Initial dealer could be determined by drawing lowest card
initialState :: GameState
initialState =
  GameState
    { dealer = A,
      deck = Card.deck,
      board = Board (Map.fromList [(A, 0), (B, 0)]),
      play = Initial (Map.fromList [(A, []), (B, [])])
    }

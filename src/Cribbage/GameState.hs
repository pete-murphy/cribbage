{-# LANGUAGE DuplicateRecordFields #-}

module Cribbage.GameState where

import Card (Card)
import Data.Map (Map)
import GHC.Generics (Generic)

data Initial
  = Initial
      { deck :: [Card],
        board :: Board
      }
  deriving (Generic)

data Deal
  = Deal
      { deck :: [Card],
        board :: Board,
        -- | Dealer is determined by each player drawing a card from deck, lower
        -- card gets to deal
        dealer :: Player,
        playerHands :: PlayerHands
      }
  deriving (Generic)

data Crib
  = Crib
      { deck :: [Card],
        board :: Board,
        dealer :: Player,
        playerHands :: PlayerHands,
        -- | Players discard two cards each to the crib
        crib :: [Card]
      }
  deriving (Generic)

data PrePlay
  = PrePlay
      { deck :: [Card],
        board :: Board,
        dealer :: Player,
        playerHands :: PlayerHands,
        crib :: [Card],
        -- | The cut card
        starter :: Card
      }
  deriving (Generic)

data Play
  = Play
      { deck :: [Card],
        board :: Board,
        dealer :: Player,
        playerHands :: PlayerHands,
        crib :: [Card],
        starter :: Card,
        cardsInPlay :: CardsInPlay
      }
  deriving (Generic)

-- Fixing the number of players to two for now
data Player = A | B
  deriving (Eq, Ord, Show)

newtype Board
  = Board (Map Player Int)
  deriving (Generic)

type PlayerHands = Map Player [Card]

-- | Player can be skipped, and we need to recover the player after cards have
-- been played. Last played card will be at top of stack / head of list.
type CardsInPlay =
  [(Player, Maybe Card)]

data GameState m
  = GameState
      { initial :: m Initial,
        deal :: Initial -> m Deal,
        crib :: Deal -> m Crib,
        cutDeck :: Crib -> m PrePlay,
        play :: Play -> m Player
      }

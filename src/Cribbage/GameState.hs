{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cribbage.GameState where

import Card (Card)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map (Map)
import GHC.Generics (Generic)

newtype Initial = Initial
  { deck :: [Card]
  }
  deriving (Generic)

data Deal = Deal
  { deck :: [Card],
    -- | Dealer is determined by each player drawing a card from deck, lower
    -- card gets to deal
    dealer :: Player,
    playerHands :: PlayerHands,
    board :: Board
  }
  deriving (Generic)

data Crib = Crib
  { deck :: [Card],
    dealer :: Player,
    playerHands :: PlayerHands,
    -- | Players discard two cards each to the crib
    crib :: [Card],
    board :: Board
  }
  deriving (Generic)

data PrePlay = PrePlay
  { dealer :: Player,
    playerHands :: PlayerHands,
    crib :: [Card],
    -- | The cut card
    starter :: Card,
    board :: Board
  }
  deriving (Generic)

data Play = Play
  { board :: Board,
    dealer :: Player,
    playerHands :: PlayerHands,
    crib :: [Card],
    starter :: Card,
    cardsInPlay :: CardsInPlay,
    board :: Board
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

data Game m = Game
  { initial :: m Initial,
    deal :: Initial -> m Deal,
    makeCrib :: Deal -> m Crib,
    cutDeck :: Crib -> m PrePlay,
    play :: PrePlay -> m Play,
    done :: Play -> m Player -- Not sure about these last two
  }

runGame :: MonadIO m => Game m -> m Player
runGame Game {initial, deal, makeCrib, cutDeck, play, done} =
  initial >>= deal >>= makeCrib >>= cutDeck >>= play >>= done
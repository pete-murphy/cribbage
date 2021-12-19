module Cribbage.Game where

import Card (Card, Rank (..))
import qualified Card
import qualified Control.Exception as Exception
import Control.Lens.Combinators hiding (_Unwrapped)
import Control.Lens.Operators
import qualified Data.Foldable as Foldable
import Data.Generics.Labels ()
import Data.Generics.Wrapped (_Unwrapped)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import qualified Shuffle.List
import System.Random (RandomGen)
import qualified System.Random as Random
import qualified Utils.List as List

-- Fixing the number of players to two for now
data Player = A | B
  deriving (Eq, Ord, Show)

newtype Board
  = Board (Map Player Int)
  deriving (Generic)

type PlayerHands = Map Player [Card]

type Crib = [Card]

-- Player can be skipped, and we need to recover the player after cards have
-- been played. Last played card will be at top of stack / head of list.
type CardsInPlay =
  [(Player, Maybe Card)]

data PlayState
  = PlayState
      { playerHands :: PlayerHands,
        crib :: Maybe Crib,
        cutCard :: Maybe Card,
        cardsInPlay :: Maybe CardsInPlay
      }
  deriving (Generic, Show)

data GameState
  = GameState
      { dealer :: Maybe Player,
        deck :: [Card],
        board :: Board,
        play :: PlayState
      }
  deriving (Generic)

initialState :: RandomGen g => g -> GameState
initialState gen =
  GameState
    { -- TODO: Initial dealer could be determined by drawing lowest card
      dealer = Just A,
      deck = Shuffle.List.shuffle gen Card.deck,
      board = Board (Map.fromList [(A, 0), (B, 0)]),
      -- play = Initial (Map.fromList [(A, []), (B, [])])
      play = PlayState (Map.fromList [(A, []), (B, [])]) Nothing Nothing Nothing
    }

-- | We can always derive the active player from current game state by looking
-- at who played the last card (or if there are no cards played, its the
-- dealer's turn)
activePlayer :: GameState -> Player
activePlayer gameState =
  case gameState ^? failing (#play . #cardsInPlay . _Just . ix 0 . _1) (#dealer . _Just) of
    Just player -> player
    _ -> error "Failed to get active player"

otherPlayer :: Player -> Player
otherPlayer = \case
  A -> B
  B -> A

deal ::
  Int -> -- How many to deal (could fix to 6)
  GameState ->
  GameState
deal n gameState =
  case List.splitAtMany [n, n] (deck gameState) of
    Just [a, b, rest] ->
      gameState
        & #play . #playerHands . partsOf traversed .~ [a, b]
        & #deck .~ rest
    _ -> error "Not enough cards to deal"

makeCrib ::
  [Card] -> -- Which cards player A chooses to put in crib
  [Card] -> -- Which cards player B chooses to put in crib
  GameState ->
  GameState
makeCrib cribA cribB gameState =
  case gameState ^. #play . #playerHands . to Map.elems of
    [ List.select cribA -> Just restA,
      List.select cribB -> Just restB
      ] ->
        gameState
          -- `#play . #playerHands . traversed` would be a
          -- `Traversal' GameState [Card]`
          -- `partsOf` makes that a `Lens' GameState [[Card]]`
          -- so we can update both hands at the same time
          & #play . #playerHands . partsOf traversed .~ [restA, restB]
          & #play . #crib ?~ cribA <> cribB
    _ -> error "Failed to make crib"

cutDeck :: GameState -> GameState
cutDeck gameState =
  let dealer = activePlayer gameState
   in case List.selectIxs [0] (deck gameState) of
        Just ([cutCard], rest) ->
          gameState
            & #deck .~ rest
            & #play . #cutCard ?~ cutCard
            -- If cut card is Jack, dealer gets two points ("his heels")
            & #board . _Unwrapped . ix dealer . filtered (\_ -> Card.rank cutCard == Jack) +~ 2
        _ -> error "Failed to cut deck"

-- TODO: Update score on card play
playCard :: Card -> GameState -> GameState
playCard card gameState =
  let player = activePlayer gameState
      sumOfCardsInPlay =
        sumOf
          (#play . #cardsInPlay . _Just . folded . _2 . _Just . to Card.value)
          gameState
      canPlayCard = sumOfCardsInPlay + Card.value card <= 31
   in case (List.select [card] (gameState ^. #play . #playerHands . ix player), canPlayCard) of
        (Just rest, True) ->
          gameState
            & #play . #playerHands . ix player .~ rest
            & #play . #cardsInPlay . _Just %~ (:) (player, Just card)
        _ -> error "Failed to play card"

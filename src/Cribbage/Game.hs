module Cribbage.Game where

import Card (Card)
import qualified Card
import qualified Control.Exception as Exception
import Control.Lens.Combinators
import Control.Lens.Operators
import qualified Data.Foldable as Foldable
import Data.Generics.Labels ()
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

type PlayerHands = Map Player [Card]

type Crib = [Card]

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
      { dealer :: Player,
        deck :: [Card],
        board :: Board,
        play :: PlayState
      }
  deriving (Generic)

initialState :: RandomGen g => g -> GameState
initialState gen =
  GameState
    { -- TODO: Initial dealer could be determined by drawing lowest card
      dealer = A,
      deck = Shuffle.List.shuffle gen Card.deck,
      board = Board (Map.fromList [(A, 0), (B, 0)]),
      -- play = Initial (Map.fromList [(A, []), (B, [])])
      play = PlayState (Map.fromList [(A, []), (B, [])]) Nothing Nothing Nothing
    }

deal ::
  Int -> -- How many to deal (could fix to 6)
  GameState ->
  GameState
deal n gameState = case List.splitAtMany [n, n] (deck gameState) of
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
          -- Just `#play . #playerHands . traversed` would be a
          -- `Traversal' GameState [Card]`
          -- `partsOf` makes that a `Lens' GameState [[Card]]`
          -- so we can update both hands at the same time
          & #play . #playerHands . partsOf traversed .~ [restA, restB]
          & #play . #crib ?~ cribA <> cribB
    _ -> error "Failed to make crib"

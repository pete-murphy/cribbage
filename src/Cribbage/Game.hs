module Cribbage.Game where

import Card (Card, Rank (..))
import qualified Card
import qualified Control.Exception as Exception
import Control.Lens.Combinators hiding (_Unwrapped)
import Control.Lens.Operators
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as Reader
import Cribbage.GameState
  ( Board (..),
    Crib (..),
    Deal (..),
    GameState (GameState),
    Initial (..),
    Play (..),
    Player (..),
    PrePlay (..),
  )
import qualified Data.Foldable as Foldable
import Data.Generics.Labels ()
import Data.Generics.Wrapped (_Unwrapped)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import GHC.TypeLits (Nat)
import qualified Shuffle.List
import System.Random (RandomGen, StdGen)
import qualified System.Random as Random
import qualified Utils.List as List

initial :: MonadIO m => m Initial
initial = do
  gen <- liftIO Random.newStdGen
  pure
    ( Initial
        { deck = Shuffle.List.shuffle gen Card.deck,
          board = Board (Map.fromList [(A, 0), (B, 0)])
        }
    )

gameState :: MonadIO m => GameState m
gameState =
  GameState initial undefined undefined undefined undefined

-- |
-- -- | We can always derive the active player from current game state by looking
-- -- at who played the last card (or if there are no cards played, its the
-- -- dealer's turn)
-- activePlayer :: GameState -> Player
-- activePlayer gameState =
--   case gameState ^? failing (#play . #_Play . position @4 . ix 0 . _1) (#dealer . _Just) of
--     Just player -> player
--     _ -> error "Failed to get active player"

-- otherPlayer :: Player -> Player
-- otherPlayer = \case
--   A -> B
--   B -> A

-- deal ::
--   Int -> -- How many to deal (could fix to 6)
--   GameState ->
--   GameState
-- deal n gameState =
--   case List.splitAtMany [n, n] (deck gameState) of
--     Just [a, b, rest] ->
--       gameState
--         & #play . playerHands . partsOf traversed .~ [a, b]
--         & #deck .~ rest
--     _ -> error "Not enough cards to deal"

-- makeCrib ::
--   [Card] -> -- Which cards player A chooses to put in crib
--   [Card] -> -- Which cards player B chooses to put in crib
--   GameState ->
--   GameState
-- makeCrib cribA cribB gameState =
--   case gameState ^. #play . playerHands . to Map.elems of
--     [ List.select cribA -> Just restA,
--       List.select cribB -> Just restB
--       ] ->
--         gameState
--           -- `#play . #playerHands . traversed` would be a
--           -- `Traversal' GameState [Card]`
--           -- `partsOf` makes that a `Lens' GameState [[Card]]`
--           -- so we can update both hands at the same time
--           & #play . playerHands . partsOf traversed .~ [restA, restB]
--           & #play . #_Crib . position @2 .~ cribA <> cribB
--     _ -> error "Failed to make crib"

-- cutDeck :: GameState -> GameState
-- cutDeck gameState =
--   let dealer = activePlayer gameState
--    in case List.selectIxs [0] (deck gameState) of
--         Just ([cutCard], rest) ->
--           gameState
--             & #deck .~ rest
--             & #play . #_CutCard . position @3 .~ cutCard
--             -- If cut card is Jack, dealer gets two points ("his heels")
--             & #board . _Unwrapped . ix dealer . filtered (\_ -> Card.rank cutCard == Jack) +~ 2
--         _ -> error "Failed to cut deck"

-- -- TODO: Update score on card play
-- playCard :: Card -> GameState -> GameState
-- playCard card gameState =
--   let player = activePlayer gameState
--       sumOfCardsInPlay =
--         sumOf
--           (#play . #_Play . position @4 . folded . _2 . _Just . to Card.value)
--           gameState
--       canPlayCard = sumOfCardsInPlay + Card.value card <= 31
--    in case (List.select [card] (gameState ^. #play . playerHands . ix player), canPlayCard) of
--         (Just rest, True) ->
--           gameState
--             & #play . playerHands . ix player .~ rest
--             & #play . #_Play . position @4 %~ (:) (player, Just card)
--         _ -> error "Failed to play card"

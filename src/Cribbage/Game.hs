{-# LANGUAGE NamedFieldPuns #-}

module Cribbage.Game where

import Card (Card, Rank (..))
import qualified Card
import Control.Lens.Combinators hiding (_Unwrapped)
import Control.Lens.Operators
import Control.Monad.IO.Class (MonadIO (..))
import Cribbage.GameState
  ( Board (..),
    Crib (..),
    Deal (..),
    Game (Game),
    Initial (..),
    Play (..),
    Player (..),
    PrePlay (..),
  )
import qualified Cribbage.GameState as GameState
import qualified Data.Foldable as Foldable
import Data.Generics.Labels ()
import Data.Generics.Wrapped (_Unwrapped)
import qualified Data.List as List
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
  let deck = Shuffle.List.shuffle gen Card.deck
  pure (Initial {deck})

deal :: MonadIO m => Initial -> m Deal
deal Initial {deck} = do
  liftIO (putStrLn "Pick a card from deck")
  -- TODO: accept a number index here, for drawing from deck
  _ <- liftIO getLine
  -- TODO: actually draw from deck, put back in and shuffle
  let (drawnA : drawnB : _) = deck
      -- TODO: if equal, draw again
      dealer = if drawnA < drawnB then A else B
      (handA, deck') = List.splitAt 4 deck
      (handB, deck'') = List.splitAt 4 deck'
      playerHands = Map.fromList [(A, handA), (B, handB)]
      board = Board (Map.fromList [(A, 0), (B, 0)])
  pure (Deal {deck = deck'', dealer, playerHands, board})

makeCrib :: MonadIO m => Deal -> m Crib
makeCrib Deal {board, deck, dealer, playerHands} = do
  liftIO (putStrLn (if dealer == A then "Pick cards for your crib" else "Pick cards for opponent's crib"))
  -- TODO: needs to be two numbers
  n <- read <$> liftIO getLine
  m <- read <$> liftIO getLine
  -- TODO: handle error here
  let (Just (cribA, handA)) = List.selectIxs [n, m] (playerHands ^. ix A)
      -- TODO: handle error here, and use diff indices
      (Just (cribB, handB)) = List.selectIxs [n, m] (playerHands ^. ix B)
      playerHands' = Map.fromList [(A, handA), (B, handB)]
      crib = cribA <> cribB
  pure (Crib {board, deck, dealer, playerHands = playerHands', crib})

cutDeck :: MonadIO m => Crib -> m PrePlay
cutDeck Crib {board, deck, dealer, playerHands, crib} = do
  let (starter : _) = deck
  pure (PrePlay {board, starter, dealer, playerHands, crib})

play :: MonadIO m => PrePlay -> m Play
play PrePlay {board, starter, dealer, playerHands, crib} = do
  let cardsInPlay = []
  pure (Play {board, dealer, playerHands, crib, starter, cardsInPlay})

done :: MonadIO m => Play -> m Player
done Play {board, dealer, playerHands, crib, starter, cardsInPlay} = do
  pure A

game :: MonadIO m => Game m
game =
  Game initial deal makeCrib cutDeck play done

otherPlayer :: Player -> Player
otherPlayer = \case
  A -> B
  B -> A

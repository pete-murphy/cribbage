{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cribbage.Game where

import Card (Card)
import qualified Card
import Control.Lens (ix)
import Control.Lens.Combinators
import Control.Lens.Operators
import qualified Data.Foldable as Foldable
import Data.Generics.Labels ()
import Data.Generics.Product.Fields (field)
import Data.Generics.Sum
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import qualified Shuffle.List
import System.Random (RandomGen)
import qualified System.Random as Random
import Prelude hiding (splitAt)

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

-- data PlayState
--   = Initial
--       { playerHands :: PlayerHands
--       }
--   | Crib
--       { playerHands :: PlayerHands,
--         crib :: Crib
--       }
--   | CutCard
--       { playerHands :: PlayerHands,
--         crib :: Crib,
--         cutCard :: Card
--       }
--   | Play
--       { playerHands :: PlayerHands,
--         crib :: Crib,
--         cutCard :: Card,
--         cardsInPlay :: CardsInPlay
--       }

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

splitAts :: [Int] -> [a] -> Maybe [[a]]
splitAts lengths xs =
  if length xs > sum lengths
    then Just (go lengths xs [])
    else Nothing
  where
    go [] rest acc = acc <> [rest]
    go (l : ls) xs' acc =
      let (x, rest) = List.splitAt l xs'
       in go ls rest (x : acc)

-- Non-optics-y version
-- deal' :: GameState -> GameState
-- deal' gs =
--   let deck' = deck gs & splitAts [4,4]
--    in case deck' of
--         Just [a,b,rest] ->
--           gs
--             { deck = rest
--             , play = ({ playerHands = (Map.fromList [(A, a), (B, b)]) } (play gs))
--             }
--         _ -> gs

deal ::
  Int -> -- How many to deal (could fix to 6)
  GameState ->
  GameState
deal n gameState = case deck gameState & splitAts [n, n] of
  Just [a, b, rest] ->
    gameState
      & partsOf (#play . #playerHands . traversed) .~ [a, b]
      & #deck .~ rest
  _ -> gameState

-- TODO: player needs to be able to choose _which_ two cards go to crib (by index?)
makeCrib :: GameState -> GameState
makeCrib gameState =
  case gameState ^. #play . #playerHands . folded . to (splitAts [2]) of
    Just [cs, rest, cs', rest'] ->
      gameState
        & partsOf (#play . #playerHands . traversed) .~ [rest, rest']
        & #play . #crib ?~ cs <> cs'
    _ -> gameState

test :: IO ()
test = do
  gen <- Random.getStdGen
  let gs = initialState gen
      afterDeal = deal 6 gs
  Foldable.traverse_ print (deck afterDeal)
  putStrLn "\n\n"
  let afterCrib = makeCrib afterDeal
  Foldable.traverse_ print (playerHands (play afterCrib))
  putStrLn "\n\n"
  print (crib (play afterCrib))
-- pure ()

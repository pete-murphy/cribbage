{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Card where

import Data.Bits ((.&.))
import qualified Data.Bits as Bits
import qualified Data.Char as Char
import qualified Data.List as List
import GHC.Generics (Generic)

data Rank
  = Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  deriving (Eq, Ord, Show, Enum, Bounded)

data Suit
  = Hearts
  | Diamonds
  | Clubs
  | Spades
  deriving (Eq, Ord, Show, Enum, Bounded)

data Card = Of {rank :: Rank, suit :: Suit}
  deriving (Eq, Ord, Show, Generic)

value :: Card -> Int
value c = min (fromEnum (rank c) + 1) 10

isFlush :: [Card] -> Bool
isFlush cards = or do
  suit' <- [minBound .. maxBound]
  pure (all ((== suit') . suit) cards)

isFlush' :: [Card] -> Bool
isFlush' =
  (/= (0 :: Int))
    . foldr ((.&.) . (Bits.bit . fromEnum . suit)) (Bits.complement 0)

showSuitBits :: Suit -> String
showSuitBits Hearts = "0001"
showSuitBits Diamonds = "0010"
showSuitBits Clubs = "0100"
showSuitBits Spades = "1000"

isStraight :: [Card] -> Bool
isStraight cards =
  let ranks = map rank (List.sort cards)
      distances = zipWith distanceBetweenRanks ranks (tail ranks)
   in all (== 1) distances

isStraight' :: [Card] -> Bool
isStraight' cards = go (map rank (List.sort cards))
  where
    go [] = True
    go [_] = True
    go (c : cs) = distanceBetweenRanks c (head cs) == 1 && go cs

distanceBetweenRanks :: Rank -> Rank -> Int
distanceBetweenRanks r1 r2 =
  abs (fromEnum r1 - fromEnum r2)

deck :: [Card]
deck = do
  suit <- [minBound .. maxBound]
  rank <- [minBound .. maxBound]
  pure (rank `Of` suit)

-- For single unicode characters
-- TODO: Broken
display' :: Card -> String
display' Of {..} = pure (Char.chr (127136 + displaySuit suit * 16 + displayRank rank))
  where
    displaySuit = \case
      Spades -> 1
      Hearts -> 2
      Diamonds -> 3
      Clubs -> 4
    displayRank = \case
      Ace -> 1
      Two -> 2
      Three -> 3
      Four -> 4
      Five -> 5
      Six -> 6
      Seven -> 7
      Eight -> 8
      Nine -> 9
      Ten -> 10
      Jack -> 11
      Queen -> 13
      King -> 14

-- For two unicode characters
display :: Card -> String
display Of {..} = displayRank rank <> displaySuit suit
  where
    displaySuit = \case
      Spades -> "♠"
      Hearts -> "♥"
      Diamonds -> "♦"
      Clubs -> "♣"
    displayRank = \case
      Ace -> "A"
      Two -> "2"
      Three -> "3"
      Four -> "4"
      Five -> "5"
      Six -> "6"
      Seven -> "7"
      Eight -> "8"
      Nine -> "9"
      Ten -> "10"
      Jack -> "J"
      Queen -> "Q"
      King -> "K"

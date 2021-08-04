{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Card where

import Data.Bits ((.&.))
import qualified Data.Bits as Bits
import qualified Data.List as List

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
  deriving (Eq, Ord)

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

-- 1111 .&. -- complement 0
-- 0010 .&.
-- 0100 .&.
-- 0100 .&.
-- 0100 .&.
-- 0100 .&.
-- 0100

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

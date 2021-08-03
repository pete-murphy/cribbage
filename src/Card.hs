{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Card where

import qualified Control.Monad.ST as ST
import qualified Control.Monad.State as State
import Control.Monad.State (MonadIO (liftIO), State (..), StateT (StateT))
import Control.Monad.Trans (MonadTrans)
import Data.Bits ((.&.))
import qualified Data.Bits as Bits
import qualified Data.List as List
import System.Random (Random, RandomGen, StdGen)
import qualified System.Random as Random

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
    go [c] = True
    go (c : cs) = distanceBetweenRanks c (head cs) == 1 && go cs

distanceBetweenRanks :: Rank -> Rank -> Int
distanceBetweenRanks r1 r2 =
  abs (fromEnum r1 - fromEnum r2)

newtype RandomState a = RandomState {runRandomState :: State StdGen a}
  deriving newtype (Functor, Applicative, Monad)

newtype RandomStateT m a = RandomStateT {runRandomStateT :: StateT StdGen m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

-- random :: RandomGen g => g -> (a, g)
rand :: Random a => RandomState a
rand = RandomState do
  (x, g) <- State.gets Random.random
  State.put g
  pure x

rand' :: (Monad m, Random a) => RandomStateT m a
rand' = RandomStateT do
  (x, g) <- State.gets Random.random
  State.put g
  pure x

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle gen = go gen []
  where
    go gen' acc [] = acc
    go gen' acc cards =
      let (n, gen'') = Random.next gen'
          (cards', pick : cards'') = List.splitAt (n `mod` length cards) cards
       in go gen'' (pick : acc) (cards' ++ cards'')

-- shuffle' :: RandomGen g => g -> Int -> [a] -> [a]
-- shuffle' gen n cards =
--           let len = length cards
--            in

shuffle' :: StdGen -> [a] -> [a]
shuffle' g = flip State.evalState g . runRandomState . go []
  where
    go :: [a] -> [a] -> RandomState [a]
    go acc [] = pure acc
    go acc cards = do
      n <- rand
      let (cards', pick : cards'') = List.splitAt (n `mod` length cards) cards
      go (pick : acc) (cards' ++ cards'')

shuffle'' :: forall m a. Monad m => StdGen -> [a] -> m [a]
shuffle'' g = flip State.evalStateT g . runRandomStateT . go []
  where
    go :: [a] -> [a] -> RandomStateT m [a]
    go acc [] = pure acc
    go acc cards = do
      n <- rand'
      let (cards', pick : cards'') = List.splitAt (n `mod` length cards) cards
      go (pick : acc) (cards' ++ cards'')

shuffle''' :: MonadIO m => [a] -> m [a]
shuffle''' cards = do
  g <- liftIO Random.newStdGen
  shuffle'' g cards

-- f(x) = 1x + 3
sub :: Int -> Int -> Int
-- sub x x = 0
sub x y = x - y
-- eq x y = False

-- nthEvenNum

-- TODO: Shuffle w
-- - DList
-- - STArray
-- - (Mutable)Vector (works in ST)

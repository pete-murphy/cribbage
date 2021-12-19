{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ViewPatterns #-}

module Cribbage.Game where

import Card (Card)
import qualified Card
import qualified Control.Exception as Exception
import Control.Lens (ix)
import Control.Lens.Combinators
import Control.Lens.Operators
import qualified Control.Monad as Monad
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

--
-- List utils

splitAts :: [Int] -> [a] -> Maybe [[a]]
splitAts lengths xs =
  if length xs > sum lengths
    then Just (go lengths xs [])
    else Nothing
  where
    go [] xs' acc = acc <> [xs']
    go (l : ls) xs' acc =
      let (x, rest) = List.splitAt l xs'
       in go ls rest (x : acc)

selectIxs :: [Int] -> [a] -> Maybe ([a], [a])
selectIxs ixs xs =
  if all (\ix -> length xs > ix) ixs
    then Just (matched, rest)
    else Nothing
  where
    matched = map (xs !!) ixs
    rest = do
      (i, x) <- List.zip [0 ..] xs
      Monad.guard (i `notElem` ixs)
      pure x

-- Kind of like a "strict difference"? If the first list is not a subset of
-- second list, this fails, otherwise it returns all elements of the second
-- list that aren't elements of the first list
select :: Eq a => [a] -> [a] -> Maybe [a]
select xs ys =
  if all (`elem` ys) xs
    then Just (filter (`notElem` xs) ys)
    else Nothing

--

deal ::
  Int -> -- How many to deal (could fix to 6)
  GameState ->
  GameState
deal n gameState = case deck gameState & splitAts [n, n] of
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
makeCrib crib crib' gameState =
  case gameState ^. #play . #playerHands . to Map.elems of
    [select crib -> Just rest, select crib' -> Just rest'] ->
      gameState
        & #play . #playerHands . partsOf traversed .~ [rest, rest']
        & #play . #crib ?~ crib <> crib'
    _ -> error "Failed to make crib"

test :: Int -> IO ()
test n = do
  -- gen <- Random.getStdGen
  let gen = Random.mkStdGen n
      gs = initialState gen
      afterDeal = deal 6 gs
  putStrLn "Deck after deal:"
  Foldable.traverse_ (putStr . (<> " ") . Card.display) (deck afterDeal)
  ---
  lineBreak
  debug "A hand before deal" (gs ^. #play . #playerHands . ix A)
  debug "B hand before deal" (gs ^. #play . #playerHands . ix B)
  debug "A hand after deal" (afterDeal ^. #play . #playerHands . ix A)
  debug "B hand after deal" (afterDeal ^. #play . #playerHands . ix B)
  ---
  lineBreak
  let cribA = afterDeal ^.. #play . #playerHands . ix A . taking 2 folded
      cribB = afterDeal ^.. #play . #playerHands . ix B . taking 2 folded
      afterCrib = makeCrib cribA cribB afterDeal
  debug "cribA" cribA
  debug "cribB" cribB
  ---
  lineBreak
  debug "A hand after crib" (afterCrib ^. #play . #playerHands . ix A)
  debug "B hand after crib" (afterCrib ^. #play . #playerHands . ix B)
  ---
  lineBreak
  Foldable.traverse_ (debug "final crib") (crib (play afterCrib))
  where
    debug msg a = putStrLn (msg <> ":\n" <> (Card.display =<< a))
    lineBreak = putStrLn "\n"

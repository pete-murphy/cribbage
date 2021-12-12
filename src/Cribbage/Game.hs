module Cribbage.Game where

import Card (Card)
import Data.Map (Map)

data Player = A | B

newtype Board
  = Board (Int, Int)

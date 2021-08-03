{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad.Except (ExceptT (..), MonadIO (liftIO), runExceptT, when)
import Data.Traversable (for)
import Data.Foldable (for_)

type Error = String
type M a = ExceptT Error IO a

newtype User = User String deriving (Show)
newtype Order = Order String

getAllUsersFromMongo :: M [User]
getAllUsersFromMongo = pure [User "user1", User "user2"]

getOrdersForUser :: User -> M [Order]
getOrdersForUser (User u) = pure [Order (u <> "-order1")]

sendDiscountEmail :: User -> M ()
sendDiscountEmail u = ExceptT (pure (Left ("Failed to send email for user: " <> show u)))

main :: IO ()
main = runExceptT program >>= \case
  Left err -> print err
  Right x -> print x

program :: M String
program = do
  allUsers <- getAllUsersFromMongo
  usersWithOrders <- for allUsers \user -> do
    (user,) <$> getOrdersForUser user
  for_ usersWithOrders \(user, orders) -> do
    when (length orders <= 1) (sendDiscountEmail user)
  pure "Success"

{-# LANGUAGE GADTs #-}
module UserActivity (userActivity, createUser) where

import Types
import Control.Monad
import Control.Concurrent
import System.Random
import qualified Data.Map as Map

-- Function to create a user with a given name and an empty mail box which will store received messages
createUser :: String -> IO User
createUser name = do
  mail_box <- newMVar []
  return $ User name mail_box

-- Function to send a message from one user to another
sendMessage :: User -> User -> Message -> IO ()
sendMessage fromUser toUser message = do
  modifyMVar_ (receivedMessages toUser) (\messages -> return $ message : messages)

-- Function to simulate user activity
userActivity :: [User] -> User -> IO ()
userActivity users currentUser = replicateM_ 100 $ do
  -- Simulate random time intervals
  threadDelay =<< randomRIO (100000, 500000)

  -- Pick a random user to send a message to
  recipientIndex <- randomRIO (0, length users - 1)
  let recipient = users !! recipientIndex
  let sender = username currentUser

  -- Create and send a message
  let messageContent = "Hi! How are you? This is  " ++ sender
  let message = Message sender messageContent
  sendMessage currentUser recipient message
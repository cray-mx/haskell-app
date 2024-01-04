{-# LANGUAGE BangPatterns #-}

import Control.Concurrent
import Control.Monad
import System.Random
import qualified Data.Map as Map

-- Define the User and Message types
data User = User {
  username :: String,
  receivedMessages :: MVar [Message]
}

data Message = Message {
  fromUser :: String,
  content :: String
}

instance Show Message where
  show (Message from content) = from ++ ": " ++ content

-- Function to create a user with a given username
createUser :: String -> IO User
createUser name = do
  mbox <- newMVar []
  return $ User name mbox

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
  let messageContent = "Hello from " ++ sender
  let message = Message sender messageContent
  sendMessage currentUser recipient message

-- Main function to start the social network simulation
main :: IO ()
main = do
  -- Create 10 users
  users <- replicateM 10 $ do
    name <- replicateM 5 (randomRIO ('a', 'z'))
    createUser name

  -- Spawn threads for each user
  mapM_ (forkIO . userActivity users) users

  -- Wait for all messages to be sent
  threadDelay 10000000

  -- Output the final count of messages each user received
  forM_ users $ \user -> do
    messages <- readMVar (receivedMessages user)
    putStrLn $ username user ++ " received " ++ show (length messages) ++ " messages."
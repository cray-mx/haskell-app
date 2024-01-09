{-# LANGUAGE BangPatterns #-}

import Control.Concurrent
import Control.Monad
import UserActivity (userActivity, createUser)
import Types

-- Main function to start the social network simulation
main :: IO ()
main = do
  -- Create 10 users
  let usernames = ["steve", "paul", "bob", "alice", "jane", "rayaan", "carl", "kendall", "george", "helen"]
  users <- forM usernames $ \name -> createUser name

  -- Spawn threads for each user
  mapM_ (forkIO . userActivity users) users

  -- Wait for all messages to be sent
  threadDelay 10000000

  -- Output the final count of messages each user received
  forM_ users $ \user -> do
    messages <- readMVar (receivedMessages user)
    putStrLn $ username user ++ " received " ++ show (length messages) ++ " messages."
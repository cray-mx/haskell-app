{-# LANGUAGE GADTs #-}
module Types (User(..), Message(..)) where 

import Control.Concurrent

instance Show Message where
  show (Message from content) = from ++ ": " ++ content

-- Define the User type
data User = User {
  username :: String, 
  receivedMessages :: MVar [Message]
}

-- Define the Message type
data Message = Message {
  fromUser :: String,
  content :: String
}
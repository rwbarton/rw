module Crawl.AccountInfo where

data AccountInfo = AccountInfo {
  username :: String,
  password :: String,
  server :: String,
  port :: Int,
  game_id :: String
  } deriving (Read)

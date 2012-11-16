module Crawl.AccountInfo where

data AccountInfo = AccountInfo {
  username :: String,
  password :: String,
  server :: String,
  game_id :: String
  } deriving (Read)
